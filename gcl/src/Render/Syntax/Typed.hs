{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Syntax.Typed where

import Render.Class
import Render.Element
import Render.Syntax.Abstract hiding (handleExpr)
import Render.Syntax.Common ()
import Syntax.Common
  ( ArithOp (..),
    Fixity (..),
    Op (..),
    classify,
  )
import Syntax.Typed
import Syntax.Typed.Reduce
  ( RZ,
    currentPath,
    descend,
    initRZ,
    isRedex,
    redexRT,
  )

------------------------------------------------------------------------------

-- | Expr
instance Render Expr where
  renderPrec = handleExpr

handleExpr :: PrecContext -> Expr -> Inlines
handleExpr _ (Lit x _ l) = tempHandleMaybeRange l $ render x
handleExpr _ (Var x _ l) = tempHandleMaybeRange l $ render x
handleExpr _ (Const x _ l) = tempHandleMaybeRange l $ render x
handleExpr _ (Op _ _) = error "erroneous syntax given to render"
handleExpr _ (Chain ch) = render ch
handleExpr n (App (App (Op (ArithOp op) _) left _) right _) =
  -- binary operators
  parensIf n (Just (ArithOp op)) $
    renderPrec (HOLEOp (ArithOp op)) left
      <+> render op
      <+> renderPrec (OpHOLE (ArithOp op)) right
handleExpr n (App (Op (ArithOp op) _) e _) = case classify (ArithOp op) of -- unary operators, this case shouldn't be former than the binary case
  (Prefix, _) -> parensIf n (Just (ArithOp op)) $ render op <+> renderPrec (OpHOLE (ArithOp op)) e
  (Postfix, _) -> parensIf n (Just (ArithOp op)) $ renderPrec (HOLEOp (ArithOp op)) e <+> render op
  _ -> error "erroneous syntax given to render"
handleExpr n (App f e _) =
  -- should only be normal applications
  parensIf n Nothing $ renderPrec HOLEApp f <+> renderPrec AppHOLE e
handleExpr prec (Lam p t q _) =
  let ifparens = case prec of
        NoContext -> id
        _ -> parensE
   in ifparens $ "λ (" <+> render p <+> " : " <+> render t <+> ") →" <+> render q
handleExpr _ (Tuple ps) =
  "(" <+> punctuateE "," (map render ps) <+> ")"
handleExpr n (OutT i e) =
  parensIf n Nothing ("out" <+> render i <+> renderPrec AppHOLE e)
handleExpr _ (Quant op xs r t _) =
  "⟨"
    <+> renderQOp op
    <+> renderBinders xs
    <+> ":"
    <+> render r
    <+> ":"
    <+> render t
    <+> "⟩"
  where
    renderQOp (Op (ArithOp (Conj _)) _) = "∀"
    renderQOp (Op (ArithOp (ConjU _)) _) = "∀"
    renderQOp (Op (ArithOp (Disj _)) _) = "∃"
    renderQOp (Op (ArithOp (DisjU _)) _) = "∃"
    renderQOp (Op (ArithOp (Add _)) _) = "Σ"
    renderQOp (Op (ArithOp (Mul _)) _) = "Π"
    renderQOp (Op op' _) = render op'
    renderQOp op' = render op'
    renderBinders [] = ""
    renderBinders [(x, t)] = render x <+> " : " <+> render t
    renderBinders ((x, t) : xs) =
      render x
        <+> " : "
        <+> render t
        <+> ";"
        <+> renderBinders xs
handleExpr _ (ArrIdx e1 e2 _) = render e1 <> "[" <> render e2 <> "]"
handleExpr _ (ArrUpd e1 e2 e3 _) =
  "(" <+> render e1 <+> ":" <+> render e2 <+> "↣" <+> render e3 <+> ")"
-- SCM: need to print parenthesis around e1 when necessary.
handleExpr _ (Case e cs _) = "case" <+> render e <+> "of" <+> renderManySepByComma cs -- TODO: Use semicolon instead of comma
handleExpr _ (EHole h) = render h
handleExpr n (Subst e subs) =
  parensIf n Nothing $
    handleExpr AppHOLE e
      <+> "["
      <+> renderManySepByComma vs
      <+> "\\"
      <+> renderManySepByComma es
      <+> "]"
  where
    (vs, es) = unzip subs

------------------------------------------------------------------------------

-- | Render an Expr to Inlines, threading the redexRT zipper so that redex
-- nodes get wrapped (via `redexE`) with their path. Layout mirrors
-- `handleExpr` exactly; the only addition is the per-node redex marking.
renderExprRZ :: Expr -> Inlines
renderExprRZ e = handleExprRZ NoContext (initRZ [] (redexRT e)) e

-- | Wrap with a redex marker iff the current zipper node is a redex.
markRedex :: RZ -> Inlines -> Inlines
markRedex rz ins
  | isRedex rz = redexE (currentPath rz) ins
  | otherwise = ins

-- | Descend to the i-th child's zipper (indexing follows `redexRT`).
down :: RZ -> Int -> RZ
down rz i = descend rz !! i

handleExprRZ :: PrecContext -> RZ -> Expr -> Inlines
handleExprRZ _ rz (Lit x _ l) = markRedex rz $ tempHandleMaybeRange l $ render x
handleExprRZ _ rz (Var x _ l) = markRedex rz $ tempHandleMaybeRange l $ render x
handleExprRZ _ rz (Const x _ l) = markRedex rz $ tempHandleMaybeRange l $ render x
handleExprRZ _ _ (Op _ _) = error "erroneous syntax given to render"
handleExprRZ _ rz (Chain ch) = markRedex rz $ fst (handleChainRZ rz 0 ch)
handleExprRZ n rz (App (App (Op (ArithOp op) _) left _) right _) =
  -- binary operators
  markRedex rz $
    parensIf n (Just (ArithOp op)) $
      handleExprRZ (HOLEOp (ArithOp op)) (down (down rz 0) 1) left
        <+> render op
        <+> handleExprRZ (OpHOLE (ArithOp op)) (down rz 1) right
handleExprRZ n rz (App (Op (ArithOp op) _) e _) = case classify (ArithOp op) of -- unary operators
  (Prefix, _) -> markRedex rz $ parensIf n (Just (ArithOp op)) $ render op <+> handleExprRZ (OpHOLE (ArithOp op)) (down rz 1) e
  (Postfix, _) -> markRedex rz $ parensIf n (Just (ArithOp op)) $ handleExprRZ (HOLEOp (ArithOp op)) (down rz 1) e <+> render op
  _ -> error "erroneous syntax given to render"
handleExprRZ n rz (App f e _) =
  -- normal applications
  markRedex rz $ parensIf n Nothing $ handleExprRZ HOLEApp (down rz 0) f <+> handleExprRZ AppHOLE (down rz 1) e
handleExprRZ prec rz (Lam p t q _) =
  let ifparens = case prec of
        NoContext -> id
        _ -> parensE
   in markRedex rz $ ifparens $ "λ (" <+> render p <+> " : " <+> render t <+> ") →" <+> handleExprRZ NoContext (down rz 0) q
handleExprRZ _ rz (Tuple ps) =
  markRedex rz $ "(" <+> punctuateE "," (zipWith (\i p -> handleExprRZ NoContext (down rz i) p) [0 ..] ps) <+> ")"
handleExprRZ n rz (OutT i e) =
  markRedex rz $ parensIf n Nothing ("out" <+> render i <+> handleExprRZ AppHOLE (down rz 0) e)
handleExprRZ _ rz (Quant op xs r t _) =
  markRedex rz $
    "⟨"
      <+> renderQOp op
      <+> renderBinders xs
      <+> ":"
      <+> handleExprRZ NoContext (down rz 0) r
      <+> ":"
      <+> handleExprRZ NoContext (down rz 1) t
      <+> "⟩"
  where
    renderQOp (Op (ArithOp (Conj _)) _) = "∀"
    renderQOp (Op (ArithOp (ConjU _)) _) = "∀"
    renderQOp (Op (ArithOp (Disj _)) _) = "∃"
    renderQOp (Op (ArithOp (DisjU _)) _) = "∃"
    renderQOp (Op (ArithOp (Add _)) _) = "Σ"
    renderQOp (Op (ArithOp (Mul _)) _) = "Π"
    renderQOp (Op op' _) = render op'
    renderQOp op' = render op'
    renderBinders [] = ""
    renderBinders [(x, t')] = render x <+> " : " <+> render t'
    renderBinders ((x, t') : xs') =
      render x <+> " : " <+> render t' <+> ";" <+> renderBinders xs'
handleExprRZ _ rz (ArrIdx e1 e2 _) =
  markRedex rz $ handleExprRZ NoContext (down rz 0) e1 <> "[" <> handleExprRZ NoContext (down rz 1) e2 <> "]"
handleExprRZ _ rz (ArrUpd e1 e2 e3 _) =
  markRedex rz $
    "(" <+> handleExprRZ NoContext (down rz 0) e1 <+> ":" <+> handleExprRZ NoContext (down rz 1) e2 <+> "↣" <+> handleExprRZ NoContext (down rz 2) e3 <+> ")"
handleExprRZ _ rz (Case e cs _) =
  markRedex rz $ "case" <+> handleExprRZ NoContext (down rz 0) e <+> "of" <+> punctuateE "," (zipWith clause [1 ..] cs)
  where
    clause i (CaseClause pat expr) = render pat <+> "→" <+> handleExprRZ NoContext (down rz i) expr
handleExprRZ _ rz (EHole h) = markRedex rz $ render h
handleExprRZ n rz (Subst e subs) =
  markRedex rz $
    parensIf n Nothing $
      handleExprRZ AppHOLE (down rz 0) e
        <+> "["
        <+> renderManySepByComma (map fst subs)
        <+> "\\"
        <+> punctuateE "," (zipWith (\i (_, se) -> handleExprRZ NoContext (down rz i) se) [1 ..] subs)
        <+> "]"

------------------------------------------------------------------------------

instance Render CaseClause where
  render (CaseClause pat expr) = render pat <+> "→" <+> render expr

-- | Recursive rendering of a Chain with redex marking.
-- Returns the rendered Inlines and the next child index to use.
handleChainRZ :: RZ -> Int -> Chain -> (Inlines, Int)
handleChainRZ rz i (Pure e) = (handleExprRZ NoContext (down rz i) e, i + 1)
handleChainRZ rz i (More ch op _ e) =
  let (ins, nextI) = handleChainRZ rz (i + 1) ch
   in (ins <+> render op <+> handleExprRZ NoContext (down rz i) e, nextI)

instance Render Chain where -- Hopefully this is correct.
  render (Pure expr) = render expr
  render (More ch op _ expr) = render ch <+> render op <+> render expr

instance Render Hole where
  render (Hole t holeNumber _ _ _) = "{!" <+> render t <+> "!}" <> subscriptNumber holeNumber
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
