{-# LANGUAGE OverloadedStrings #-}

module Render.Syntax.AbstractLN where

import Render.Class
import Render.Element
import Render.Syntax.Common.BuiltInOp ()
import Syntax.AbstractLN
import Syntax.Common.BuiltInOp

-- | Literals
instance Render Lit where
  render (Num i) = render (show i)
  render (Bol b) = render (show b)
  render (Chr c) = render (show c)
  render (Op op) = render op

-- | Expressions
instance Render Expr where
  renderPrec _ (Lit lit r) = tempHandleMaybeRange r $ render lit
  renderPrec _ (Var var' r) = tempHandleMaybeRange r $ render var'
  renderPrec ctx (Chain chain) = case chain of
    Pure e _ -> renderPrec ctx e
    More _ op _ _ -> parensIf ctx (Just op) (render chain)
  -- ChAoS(Render): Have to swap out OP type to BuiltInOp one in order to render it
  renderPrec _ (App (App (Lit (Op op) _) left _) right _) = undefined
  renderPrec _ (App (Lit (Op op) _) e _) = undefined
  renderPrec ctx (App f e _) =
    -- should only be normal applications
    parensIf ctx Nothing $ renderPrec HOLEApp f <+> renderPrec AppHOLE e
  renderPrec ctx (Lam p q _) = case ctx of
    NoContext -> body
    _ -> parensE body
    where
      body = "λ" <+> render p <+> "→" <+> render q
  renderPrec _ (Quant opExpr xs r t _) =
    "⟨"
      <+> renderQOp op
      <+> horzE (map render xs)
      <+> ":"
      <+> render r
      <+> ":"
      <+> render t
      <+> "⟩"
    where
      op = case opExpr of
        Lit op' _ -> op'
        _ -> error "Not a supported quantifier operator"

      renderQOp (Op Conj) = "∀"
      renderQOp (Op Disj) = "∃"
      renderQOp (Op Add) = "Σ"
      renderQOp (Op Mul) = "Π"
      renderQOp (Op op') = render op'
      renderQOp op' = render op'
  renderPrec _ (Case expr cases _) =
    "case" <+> render expr <+> "of" <+> vertE (map render cases)
  renderPrec _ (EHole hole) = render hole

-- ChAoS(Render): This doesn't reflect correct grouping.
instance Render Chain where
  render (Pure expr _) = render expr
  render (More ch op expr _) = render ch <+> render op <+> render expr

instance Render Hole where
  render (Hole text num _) = "{!" <> textE text <> "!}" <> subscriptNumber num
    where
      -- Transform number to its subscript form by convert it to ascii value then adds to
      -- the unicode subscript number section.
      subscriptNumber :: Int -> Inlines
      subscriptNumber = render . map digitToSubscript . show

      digitToSubscript :: Char -> Char
      digitToSubscript '0' = '₀'
      digitToSubscript '1' = '₁'
      digitToSubscript '2' = '₂'
      digitToSubscript '3' = '₃'
      digitToSubscript '4' = '₄'
      digitToSubscript '5' = '₅'
      digitToSubscript '6' = '₆'
      digitToSubscript '7' = '₇'
      digitToSubscript '8' = '₈'
      digitToSubscript '9' = '₉'
      digitToSubscript c = c

-- ChAoS(Render): How should we treat range in PattConstructor?
instance Render Pattern where
  renderPrec _ (PattBinder binder r) = tempHandleMaybeRange r $ render binder
  renderPrec _ (PattWildcard _) = "_"
  renderPrec _ (PattConstructor (Left (Op Pr)) _ [left, right]) =
    parensE $ (render left <> ",") <+> render right
  renderPrec ctx (PattConstructor ctor _ args) = case (ctx, ctor) of
    (NoContext, _) -> body
    (_, Left _) -> body
    (_, Right _) -> parensE body
    where
      renderedCtor = either render render ctor
      body = renderedCtor <+> horzE (map (renderPrec AppHOLE) args)

instance Render Pragma where
  render (Proof proof proofText) =
    vertE ["{-", textE proof, "---", textE proofText, "-}"]

-- | Type

-- ChAoS(Render): What's the correct notation?
instance Render Var where
  render (FName name) = render name
  render (BName idx) = "#" <> render idx

instance Render TBuiltIn where
  render TInt = "Int"
  render TBool = "Bool"
  render TChar = "Char"
  -- ChAoS(Render): failsafe fallback solutions
  render TArray = "[]"
  render TArrow = "->"
  render TPair = "(,)"

instance Render Type where
  renderPrec _ (TLit builtIn _) = render builtIn
  renderPrec _ (TVar var' _) = render var'
  renderPrec _ (TApp (TLit TArray _) ty _) = "[" <> render ty <> "]"
  -- ChAoS(Render): Have to swap out OP type to BuiltInOp one in order to render it
  renderPrec ctx (TApp (TApp (TLit TArrow _) fTy _) eTy _) = undefined
  renderPrec ctx (TApp (TApp (TLit TPair _) fTy _) eTy _) = undefined
  renderPrec ctx (TApp lTy rTy _) =
    parensIf ctx Nothing $ renderPrec HOLEApp lTy <+> renderPrec AppHOLE rTy
  renderPrec ctx (TForall binders tys _) = undefined
  renderPrec _ TType = "*"

------------------------------------------------

-- | The second argument: Nothing means the op at-issue is application.
parensIf :: PrecContext -> Maybe Op -> Inlines -> Inlines
parensIf pc mop = case isomerismOfContextAndCurrentOp pc mop of
  Nothing ->
    let conditionOfOmittingParens = case mop of
          Just Neg -> commonParenOmittingCondition
          -- negation is treated differently from other unary operators:
          -- others, e.g., minus: "a * (- b)", but negation: "a && ~b" doesn't need parentheses
          _ -> case classify' mop of
            Prefix -> case pc of
              AppHOLE -> False
              OpHOLE _ -> False
              _ -> precOfPC pc >= precOf' mop
            Postfix ->
              commonParenOmittingCondition
                || precOfPC pc == precOf' mop
            _ ->
              commonParenOmittingCondition
                || (sameOpSym' pc mop && isAssocOp' mop)
     in if conditionOfOmittingParens
          then id
          else parensE
  Just Cis ->
    -- e.g., In "a -> (b -> c)", "(a - b) - c", parentheses can be omitted.
    if commonParenOmittingCondition || sameOpSym' pc mop
      then id
      else parensE
  Just Trans ->
    -- e.g., In "a - (b - c)", the parentheses shouldn't be omitted;
    --  but in "a * (b * c)", since "*" is associative, the parentheses can be omitted.
    if commonParenOmittingCondition
      || ( sameOpSym' pc mop
             && isAssocOp' mop
         )
      then id
      else parensE
  where
    commonParenOmittingCondition =
      precOfPC pc > precOf' mop
        || (isChainPC pc && isChainOp' mop)

    isChainPC :: PrecContext -> Bool
    isChainPC pc' = case pc' of
      NoContext -> False
      AppHOLE -> False
      HOLEApp -> False
      -- ChAoS(Render): Have to swap out OP type to BuiltInOp one
      _ -> error "Not implemented yet"
    -- OpHOLE op -> isChainOp' (Just op)
    -- HOLEOp op -> isChainOp' (Just op)

    isChainOp' :: Maybe Op -> Bool
    isChainOp' = maybe False isChainOp

    -- In this scope, every "Nothing" case of "Maybe Op" means application.
    sameOpSym' :: PrecContext -> Maybe Op -> Bool
    sameOpSym' pc' Nothing = case pc' of
      AppHOLE -> True
      HOLEApp -> True
      _ -> False
    sameOpSym' pc' (Just op) = case pc' of
      -- ChAoS(Render): Have to swap out OP type to BuiltInOp one
      _ -> error "Not implemented yet"
      -- OpHOLE pcop -> pcop == op
      -- HOLEOp pcop -> pcop == op
      _ -> False

    isAssocOp' :: Maybe Op -> Bool
    isAssocOp' = maybe False isAssocOp

    precOf' :: Maybe Op -> Int
    precOf' Nothing = initOrderIndex - 1
    precOf' (Just op) = precOf op

    precOfPC :: PrecContext -> Int
    precOfPC pc' = case pc' of
      NoContext -> 99999
      AppHOLE -> precOf' Nothing
      HOLEApp -> precOf' Nothing
      -- ChAoS(Render): Have to swap out OP type to BuiltInOp one
      _ -> error "Not implemented yet"

-- OpHOLE op -> precOf op
-- HOLEOp op -> precOf op

data Isomerism = Cis | Trans deriving (Show) -- taking the concept from chemistry

-- | The Nothing in the second argument means application.
-- Recalling that application is left associative "a b c" == "(a b) c"
isomerismOfContextAndCurrentOp :: PrecContext -> Maybe Op -> Maybe Isomerism
isomerismOfContextAndCurrentOp pc mop = case pc of
  NoContext -> Nothing
  AppHOLE -> case classify' mop of
    InfixL -> Just Trans -- a - {b - c}   ==> a - (b - c)
    InfixR -> Just Cis -- a -> {b -> c} ==> a -> b -> c
    _ -> Nothing
  HOLEApp -> case classify' mop of
    InfixL -> Just Cis -- {a - b} - c   ==> a - b - c
    InfixR -> Just Trans -- {a -> b} -> c ==> (a -> b) -> c
    _ -> Nothing
  OpHOLE _ -> isomerismOfContextAndCurrentOp AppHOLE mop
  HOLEOp _ -> isomerismOfContextAndCurrentOp HOLEApp mop

classify' :: Maybe Op -> Fixity
classify' Nothing = InfixL
classify' (Just op) = fst $ classify op
