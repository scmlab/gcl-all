{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Parser.Definition where

import Control.Monad.Combinators.Expr
import qualified Data.Set as Set (singleton)
import Syntax.Common hiding (Fixity (..))
import Syntax.Concrete hiding (Op)
import Syntax.Parser.Basics
import Syntax.Parser.Expr
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

definition :: Parser Definition
definition = choice [try valDefnSig, typeDefn, valDefn]
  where
    valDefnSig :: Parser Definition
    valDefnSig = ValDefnSig <$> declBase identifier

    valDefn :: Parser Definition
    valDefn = do
      (ident, args) <- unwindLhs =<< lhsExpr
      tEq <- tokenEQ
      rhs <- expression
      return (ValDefn ident args tEq rhs)

    -- `data T a1 a2 ... = C1 ai1 ai2 .. | C2 ... | ...`
    typeDefn :: Parser Definition
    typeDefn = TypeDefn <$> tokenData <*> upper <*> many lower <*> tokenEQ <*> sepByGuardBar typeDefnCtor

    typeDefnCtor :: Parser TypeDefnCtor
    typeDefnCtor =
      TypeDefnCtor
        <$> upper
        <*> (maybe [] (reverse . unwindTApp) <$> optional type')
      where
        unwindTApp :: Type -> [Type]
        unwindTApp (TApp ts t) = t : unwindTApp ts
        unwindTApp t = [t]

    sepByGuardBar :: Parser a -> Parser (SepBy "|" a)
    sepByGuardBar = sepBy' tokenGuardBar

lhsExpr :: Parser Expr
lhsExpr = makeExprParser term opTable <?> "lhs expression"
  where
    opTable = [[InfixL (return App)]]
    term =
      choice
        [ Lit <$> literal,
          Paren <$> tokenParenOpen <*> lhsExpr <*> tokenParenClose,
          Var <$> lower,
          Const <$> upper
        ]
        <?> "lhs term"

unwindApp :: Expr -> [Expr]
unwindApp = reverse . unwindApp'
  where
    unwindApp' (Paren _ e _) = unwindApp' e
    unwindApp' (App e1 e2) = e2 : unwindApp' e1
    unwindApp' e = [e]

unwindLhs :: Expr -> Parser (Name, [Pattern])
unwindLhs e = case unwindApp e of
  (Var ident : es) -> (ident,) <$> mapM unwindPtn es
  (Const ident : es) -> (ident,) <$> mapM unwindPtn es
  _ -> failWithMsg "expecting identifier"

unwindPtn :: Expr -> Parser Pattern
unwindPtn e = case unwindApp e of
  [Lit lit] -> return (PattLit lit)
  (Lit _ : _) -> failWithMsg "literal applied to arguments"
  [Var v] -> return (PattBinder v)
  (Var _ : _) -> failWithMsg "pattern starting with a variable"
  (Const con : args) ->
    PattConstructor con <$> mapM unwindPtn args
  _ -> failWithMsg "ill-formed pattern"

failWithMsg :: (MonadParsec e s m) => String -> m a
failWithMsg msg =
  fancyFailure (Set.singleton (ErrorFail msg))

definitionBlock :: Parser DefinitionBlock
definitionBlock = DefinitionBlock <$> tokenDeclOpen <*> sepByAlignmentOrSemi definition <*> tokenDeclClose

-- `n : type`
declBase :: Parser Name -> Parser DeclBase
declBase name = DeclBase <$> sepByComma name <*> tokenColon <*> type'

-- `{ expr }`
declProp :: Parser DeclProp
declProp = DeclProp <$> tokenBraceOpen <*> expression <*> tokenBraceClose
