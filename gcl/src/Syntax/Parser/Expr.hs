{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Expr where

import Control.Monad.Combinators.Expr
import GCL.Range
import Syntax.Common hiding (Fixity (..))
import Syntax.Concrete hiding (Op)
import qualified Syntax.Concrete.Types as Expr
import Syntax.Parser.Basics
import Syntax.Parser.Lexer
import Syntax.Parser.Token
import Syntax.Parser.Types
import Syntax.Parser.Util
import Text.Megaparsec hiding
  ( ParseError,
    Pos,
    State,
    Token,
    parse,
    tokens,
  )
import Prelude hiding
  ( EQ,
    GT,
    LT,
    Ordering,
    lookup,
  )

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

predicate :: Parser Expr
predicate = expression <?> "predicate"

expression :: Parser Expr
expression = do
  makeExprParser (term <|> caseOf) opTable <?> "expression"
  where
    opTable :: [[Operator Parser Expr]]
    opTable =
      [ -- The order should be same as in Syntax.Common.Types
        [InfixL (return App)],
        [ Prefix $
            foldr1 (.)
              <$> some
                ( unary Neg TokNeg
                    <|> unary NegU TokNegU
                ),
          Prefix $ unary NegNum TokSub
        ],
        [InfixL $ arithOp Exp TokExp],
        [ InfixL $ arithOp Mod TokMod,
          InfixL $ arithOp Mul TokMul,
          InfixL $ arithOp Div TokDiv
        ],
        [ InfixL $ arithOp Add TokAdd,
          InfixL $ arithOp Sub TokSub
        ],
        [ InfixL $ arithOp Max TokMax,
          InfixL $ arithOp Min TokMin
        ],
        [ InfixN $ arithOp PointsTo TokPointsTo
        ],
        -- =
        [ InfixL $ chainOp EQ TokEQ,
          -- ~, <, <=, >, >=
          InfixL $ chainOp NEQ TokNEQ,
          InfixL $ chainOp NEQU TokNEQU,
          InfixL $ chainOp LT TokLT,
          InfixL $ chainOp LTE TokLTE,
          InfixL $ chainOp LTEU TokLTEU,
          InfixL $ chainOp GT TokGT,
          InfixL $ chainOp GTE TokGTE,
          InfixL $ chainOp GTEU TokGTEU
        ],
        --- &&
        [ InfixL $ arithOp Conj TokConj,
          InfixL $ arithOp ConjU TokConjU,
          --- ||
          InfixL $ arithOp Disj TokDisj,
          InfixL $ arithOp DisjU TokDisjU,
          InfixL $ arithOp SConj TokSConj
        ],
        -- =>
        [ InfixR $ arithOp Implies TokImpl,
          InfixR $ arithOp ImpliesU TokImplU,
          -- <=>
          InfixL $ chainOp EQProp TokEQProp,
          InfixL $ chainOp EQPropU TokEQPropU,
          InfixR $ arithOp SImp TokLolipop
        ]
      ]
      where
        arithOp :: (Maybe Range -> ArithOp) -> Tok -> Parser (Expr -> Expr -> Expr)
        arithOp operator' tok = do
          range <- symbol tok
          let op = operator' (Just range)
          return $ \x y -> App (App (Expr.Op op) x) y

        chainOp :: (Maybe Range -> ChainOp) -> Tok -> Parser (Expr -> Expr -> Expr)
        chainOp operator' tok = do
          range <- symbol tok
          let op = operator' (Just range)
          return (`makeChain` op)
          where
            makeChain a op b = Chain $ More (asChain a) op b
            asChain (Chain c) = c
            asChain e = Pure e

    unary :: (Maybe Range -> ArithOp) -> Tok -> Parser (Expr -> Expr)
    unary operator' tok = do
      range <- symbol tok
      return $ \result -> App (Expr.Op (operator' (Just range))) result

    parensExpr :: Parser Expr
    parensExpr = Paren <$> tokenParenOpen <*> expression <*> tokenParenClose

    caseOf :: Parser Expr
    caseOf = Case <$> tokenCase <*> expression <*> tokenOf <*> sepByAlignmentOrSemi1 caseClause -- blockOf caseClause
    caseClause :: Parser CaseClause
    caseClause = CaseClause <$> pattern' <*> tokenArrow <*> expression -- block expression
    term :: Parser Expr
    term =
      choice
        [ Lit <$> literal,
          try array,
          parensExpr,
          Var <$> lower,
          Const <$> upper,
          Quant
            <$> choice [Left <$> tokenQuantOpen, Right <$> tokenQuantOpenU]
            <*> (Left <$> quantifierOp)
            <*> some lower
            <*> tokenColon
            <*> expression
            <*> tokenColon
            <*> expression
            <*> choice [Left <$> tokenQuantClose, Right <$> tokenQuantCloseU],
          EHole <$> hole
        ]
        <?> "term"

    -- shoule parse A[A[i]], A[i1][i2]...[in]
    array :: Parser Expr
    array = do
      arr <- choice [parensExpr, Var <$> lower, Const <$> upper]
      indices <- some $ do
        open <- tokenBracketOpen
        xs <- term
        close <- tokenBracketClose
        return (open, xs, close)
      return $ helper arr indices
      where
        helper :: Expr -> [(Token "[", Expr, Token "]")] -> Expr
        helper a [] = a
        helper a ((o, x, c) : xs) = helper (Arr a o x c) xs

    -- The operators a quantifier may aggregate with.
    --
    -- Deliberately narrow. An aggregation needs an identity -- its range may be
    -- empty -- along with associativity and commutativity, since the range is a
    -- set and so fixes no order. The tool can only carry that table for
    -- operators it knows, and the language offers no way for a user-defined name
    -- to supply one. Hence names are refused here, as are built-in operators
    -- that do not meet those requirements.
    --
    -- Two admitted operators are not fully covered by that argument: "↑" and "↓"
    -- have no identity over unbounded Int, so what an empty range means for them
    -- is still open. They are admitted because the examples rely on them, and
    -- narrowing the slot does not make that gap worse.
    --
    -- "#" is admitted although the reasoning above does not describe it:
    -- ⟨ # x : R : T ⟩ counts rather than folds, and inferQuant gives it its own
    -- typing rule.
    --
    -- This restriction applies to surface syntax only. Abstract and typed Quant
    -- still store the operator as an arbitrary Expr, while some downstream code
    -- may rely on parser-produced operators being built-ins. Before admitting
    -- names or other forms here, audit every Quant consumer, especially
    -- free-variable/substitution handling and reduction/render path indexing.
    quantifierOp :: Parser ArithOp
    quantifierOp =
      choice
        -- Grouped by constructor, because several spellings share one: that is
        -- why "Σ" and "∃" come back out of the pretty-printer as "+" and "||".
        [ Add . Just <$> symbol TokAdd, -- "+"
          Add . Just <$> symbol TokSum, -- "Σ"
          Mul . Just <$> symbol TokMul, -- "*"
          Mul . Just <$> symbol TokProd, -- "∏"
          Conj . Just <$> symbol TokConj, -- "&&"
          Conj . Just <$> symbol TokForall, -- "∀"
          ConjU . Just <$> symbol TokConjU, -- "∧"
          Disj . Just <$> symbol TokDisj, -- "||"
          Disj . Just <$> symbol TokExist, -- "∃"
          DisjU . Just <$> symbol TokDisjU, -- "∨"
          Max . Just <$> symbol TokMax, -- "↑"
          Min . Just <$> symbol TokMin, -- "↓"
          Hash . Just <$> symbol TokHash -- "#"
        ]
        <?> "quantifier operator"

pattern' :: Parser Pattern
pattern' =
  choice
    [ PattLit <$> literal,
      PattParen <$> tokenParenOpen <*> pattern' <*> tokenParenClose,
      PattWildcard <$> tokenUnderscore,
      PattBinder <$> lower,
      PattConstructor <$> upper <*> many pattern'
    ]

hole :: Parser Hole
hole =
  choice
    [ HoleQM . rangeOf <$> tokenQuestionMark,
      Hole
        <$> tokenHoleOpen
        <*> takeWhileP (Just "anything other than '!}'") notTokHoleClose
        -- Although here we directly use mega's method instead of our 'symbol' and 'extract',
        -- it is enclosed by parsers built with 'symbol'.
        <*> tokenHoleClose
    ]
  where
    notTokHoleClose :: R Tok -> Bool
    notTokHoleClose (R _ TokHoleClose) = False
    notTokHoleClose _ = True

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

type' :: Parser Type
type' = do
  makeExprParser term table <?> "type"
  where
    table :: [[Operator Parser Type]]
    table = [[InfixL (return TApp)], [InfixR $ typeOp Arrow TokArrow]]

    typeOp :: (Maybe Range -> TypeOp) -> Tok -> Parser (Type -> Type -> Type)
    typeOp operator' tok = do
      range <- symbol tok
      let op = operator' (Just range)
      return $ \x y -> TApp (TApp (TOp op) x) y

    term :: Parser Type
    term = primTy <|> parensType <|> array <|> typeVar <?> "type term"

    parensType :: Parser Type
    parensType = TParen <$> tokenParenOpen <*> type' <*> tokenParenClose

    typeVar :: Parser Type
    typeVar = withRange $ choice [TData <$> upper, TMetaVar <$> lower]

    array :: Parser Type
    array = TArray <$> tokenArray <*> interval <*> tokenOf <*> type'

    interval :: Parser Interval
    interval = Interval <$> endpointOpening <*> tokenRange <*> endpointClosing

    endpointOpening :: Parser EndpointOpen
    endpointOpening =
      choice
        [ IncludingOpening <$> tokenBracketOpen <*> expression,
          ExcludingOpening <$> tokenParenOpen <*> expression
        ]

    endpointClosing :: Parser EndpointClose
    endpointClosing = do
      expr <- expression
      choice
        [ IncludingClosing expr <$> tokenBracketClose,
          ExcludingClosing expr <$> tokenParenClose
        ]

primTy :: Parser Type
primTy = tInt <|> tBool <|> tChar

tInt :: Parser Type
tInt = withRange $ (TBase . TInt) <$ symbol TokIntType

tBool :: Parser Type
tBool = withRange $ (TBase . TBool) <$ symbol TokBoolType

tChar :: Parser Type
tChar = withRange $ (TBase . TChar) <$ symbol TokCharType

--------------------------------------------------------------------------------

------- Records of resolving the problem of parsing, if NegNum's precedence is higher than application. ------------------

-- Handling the parsing of NegNum:
--  The problem: conventionally, NegNum is the operator with highest precedence, but in order to parse
--  Haskell-style application, the precedence becomes the lowest (at least lower than Sub),
--  otherwise, "1-2" would be parsed as "1 (-2)", which means "1" is parsed as a function.
--  (in C-style syntax, "1 (-2)" is not a valid syntax, thus the precedence of NegNum can be higher than Sub)
--  In another hand, Sub can not be higher than application, otherwise "f n-3" becomes "f (n-3)".

--  In short, precedence of NegNum should < Sub, Sub should < application,
--  but this then yields the problem that, "-1+2" now is parsed as "-(1+2)".

--  An observation, first attempt: only the first symbol needs to be considered: -a+b should be parsed as (-a)+b, instead of -(a+b)
--  We can first check if the parsed expr is really meant to be "-(a+b)", by checking if parentheses are syntactically existed;
--  if not, then we attach the minus sign to the left-most entity of the mis-parsed expr body
--  (putting "-" to "a" of the expr "a+b", so it becomes "(-a)+b")
--  ==> This approach fails at the case "a = -1", which simply raise an error, because the binding of "-" and "1" hasn't happend
--      when handling "=".

-- Second attempt: Let NegNum have higher precedence than Sub, then try to transform
-- expressions like "(1 (-2)) (-3)" back to "(1 - 2) - 3"

-- handlingMinusSignIssue :: Expr -> Expr
--   handlingMinusSignIssue expr' = case expr' of
--         -- First four cases are just passing recursively into sub-expressions.
--         Paren to ex to'                 -> Paren to (handlingMinusSignIssue ex) to'
--         Quant e e' nas to ex to' ex' e6 -> Quant e e' nas to (handlingMinusSignIssue ex) to' (handlingMinusSignIssue ex') e6
--         Case to ex to' ccs              -> Case to (handlingMinusSignIssue ex) to' (map f ccs)
--           where
--             f :: CaseClause -> CaseClause
--             f (CaseClause pat e ex') = CaseClause pat e (handlingMinusSignIssue ex')
--         Arr ex to ex' to'               -> Arr (handlingMinusSignIssue ex) to (handlingMinusSignIssue ex') to'
--         App ex1 (App neg@(Op (ArithOp (NegNum l))) ex2) ->
--           -- It has the pattern "a (-b)"
--           if isInBinaryOpContext ex1 then
--             -- Don't transform if the context is inside an operator
--             noTransformation
--           else
--             -- First check if the user really mean this, i.e., there exist parentheses:
--               -- Doesn't really need to check, because concrete syntax has "Paren", and it won't fall into this pattern
--             -- Then make it the right expression:
--             noTransformation
--           where
--             noTransformation = App (handlingMinusSignIssue ex1) (App neg $ handlingMinusSignIssue ex2)
--             transformation =
--               App (App (Op (ArithOp (Sub l))) (handlingMinusSignIssue ex1)) (handlingMinusSignIssue ex2)

--             isInBinaryOpContext :: Expr -> Bool
--             isInBinaryOpContext (App (Op op) _) = isBinary op
--             isInBinaryOpContext _ = False
--             isBinary :: Op -> Bool
--             isBinary op = case fst (classify op) of
--               Operator.Infix -> True
--               Operator.InfixR -> True
--               Operator.InfixL -> True
--               Operator.Prefix -> False
--               Operator.Postfix -> False
--         App ex ex' -> App (handlingMinusSignIssue ex) (handlingMinusSignIssue ex')
--         ex@_ -> ex

------- Records of the former method handling minus sign in expression parsing.
-- let expr' = case expr of
--         App neg@(Expr.Op (ArithOp (NegNum _))) e ->
--           if isntWrappedByParens e
--           then
--             addNegToLeftMost e
--           else
--             expr
--           where
--             isntWrappedByParens Paren {} = False
--             isntWrappedByParens _ = True

--             addNegToLeftMost (App (App op@(Expr.Op _) left) right) =
--               -- ChainOps (which are all binary) and binary ArithOp operators.
--               -- Unary oprators won't fall into this case, because we pattern-matched two layers of App;
--               -- and unary operators are same as the normal (App left right) case, no need to break further break it.
--               App (App op (addNegToLeftMost left)) right
--             addNegToLeftMost (App left right) = App (addNegToLeftMost left) right
--             addNegToLeftMost x = App neg x
--         _ -> expr
