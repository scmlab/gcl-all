{-# LANGUAGE OverloadedStrings #-}

module Syntax.AbstractLN.Scope where

import Control.Arrow (first, second)
import Data.Text (Text)
-- import GCL.Range (MaybeRanged (maybeRangeOf), (<--->), rangeEnd)
import Syntax.AbstractLN.Types
-- import Syntax.Common
--   ( nameToText,
--   )


instance LocallyNameless Type where
  close _ lit@TLit{} = lit
  close xis v@(TVar (FName x) l) =
     maybe v (\i -> TVar (BName i) l) (lookup x xis)
  close _ v@(TVar (BName _) _) = v
  close xis (TApp t0 t1 l) = TApp (close xis t0) (close xis t1) l
  close xis (TForall xs t l) = TForall xs (close xis' t) l
    where xis' = map (second (length xs +)) xis
  close _ TType = TType

  open _ lit@TLit{} = lit
  open _ v@(TVar (FName _) _) = v
  open ixs v@(TVar (BName i) l) =
     maybe v (\x -> TVar (FName x) l) (lookup i ixs)
  open ixs (TApp t0 t1 l) = TApp (open ixs t0) (open ixs t1) l
  open ixs (TForall xs t l) = TForall xs (open ixs' t) l
     where ixs' = map (first (length xs +)) ixs
  open _ TType = TType

instance LocallyNameless Expr where

  close _ lit@Lit{} = lit
  close xis v@(Var (FName x) l) =
     maybe v (\i -> Var (BName i) l) (lookup x xis)
  close _ v@(Var (BName _) _) = v
  close xis (Chain ch) = Chain (close xis ch)
  close xis (App e0 e1 l) = App (close xis e0) (close xis e1) l
  close xis (Lam x e l) = Lam x (close xis' e) l
     where xis' = map (second (1+)) xis
  close xis (Quant op xs ran body l) =
     Quant (close xis op) xs (close xis' ran) (close xis' body) l
    where xis' = map (second (length xs +)) xis
  close xis (Case m cls l) =
     Case (close xis m) (map (closeCl xis) cls) l
    where closeCl xis (ptn, e) = (ptn, close xis' e)
            where xs = extractBinders ptn
                  xis' = map (second (length xs +)) xis
  close _ h@EHole{} = h

  open _ lit@Lit{} = lit
  open _ v@(Var (FName _) _) = v
  open ixs v@(Var (BName i) l) =
     maybe v (\x -> Var (FName x) l) (lookup i ixs)
  open ixs (Chain ch) = Chain (open ixs ch)
  open ixs (App e0 e1 l) = App (open ixs e0) (open ixs e1) l
  open ixs (Lam x e l) = Lam x (open ixs' e) l
     where ixs' = map (first (1+)) ixs
  open ixs (Quant op xs ran body l) =
     Quant (open ixs op) xs (open ixs' ran) (open ixs' body) l
    where ixs' = map (first (length xs +)) ixs
  open ixs (Case m cls l) =
     Case (open ixs m) (map (openCl ixs) cls) l
    where openCl ixs (ptn, e) = (ptn, open ixs' e)
            where xs = extractBinders ptn
                  ixs' = map (first (length xs +)) ixs
  open _ h@EHole{} = h

instance LocallyNameless Chain where

  close xis (Pure e l) = Pure (close xis e) l
  close xis (More ch op e l) = More (close xis ch) op (close xis e) l

  open ixs (Pure e l) = Pure (open ixs e) l
  open ixs (More ch op e l) = More (open ixs ch) op (open ixs e) l

extractBinders :: Pattern -> [Text]
extractBinders (PattBinder x _) = [x]
extractBinders (PattWildcard _) = []
extractBinders (PattConstructor _ _ xs) = xs >>= extractBinders

openQuant :: Expr -> (Expr, [Text], Expr, Expr)
openQuant (Quant op xs ran body _) =
   (op, xs, open ixs ran, open ixs body)
  where ixs = zip [0..] xs
openQuant _ = error "open: not a Quant"

quant :: Expr -> [Text] -> Expr -> Expr -> Expr
quant op xs ran body =
    Quant op xs (close xis ran) (close xis body) Nothing
  where xis = zip xs [0..]

openLam :: Expr -> (Text, Expr)
openLam (Lam x e _) = (x, open [(0,x)] e)
openLam _ = error "open: not a Lam"

lam :: Text -> Expr -> Expr
lam x e = Lam x (close [(x,0)] e) Nothing

openLams :: Expr -> ([Text], Expr)
openLams e = let (xs, e') = unwindLam e
             in (xs, open (zip [0..] xs) e')

unwindLam :: Expr -> ([Text], Expr)
unwindLam (Lam x e _) = first (x:) (unwindLam e)
unwindLam e = ([],e)

lams :: [Text] -> Expr -> Expr
lams [] e = e
lams (x:xs) e = Lam x (close [(x,0)] (lams xs e)) Nothing
