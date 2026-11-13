{-# LANGUAGE OverloadedStrings #-}

module Syntax.AbstractLN.Util where

import Control.Arrow (first, second)
import Data.Text (Text)
import GCL.Range (MaybeRanged (maybeRangeOf), (<--->), rangeEnd)
import Syntax.AbstractLN.Types
import Syntax.Common
  ( Name (..),
    TypeOp (Arrow),
  )

instance LocallyNameless Expr where

  close _ lit@Lit{} = lit
  close xis v@(Var (FName x l)) =
     maybe v (\i -> Var (BName i l)) (lookup x xis)
  close _ v@(Var (BName _ _)) = v
  close xis c@(Const (FName x l)) =
     maybe c (\i -> Const (BName i l)) (lookup x xis)
  close _ c@(Const (BName _ _)) = c
  close _ op@Op{} = op
  close xis (Chain ch) = Chain (close xis ch)
  close xis (App e0 e1 l) = App (close xis e0) (close xis e1) l
  close xis (Lam x e l) = Lam x (close xis' e) l
     where xis' = map (second (1+)) xis
  close xis (Tuple es) = Tuple (map (close xis) es)
  close xis (OutT i e) = OutT i (close xis e)
  close xis (Quant op xs ran body l) =
     Quant (close xis op) xs (close xis' ran) (close xis' body) l
    where xis' = map (second (length xs +)) xis
  close xis (ArrIdx arr idx l) =
     ArrIdx (close xis arr) (close xis idx) l
  close xis (ArrUpd arr idx val l) =
     ArrUpd (close xis arr) (close xis idx) (close xis val) l
  close xis (Case m cls l) =
     Case (close xis m) (map (close xis) cls) l
  close _ h@EHole{} = h

  open _ lit@Lit{} = lit
  open _ v@(Var (FName _ _)) = v
  open ixs v@(Var (BName i l)) =
     maybe v (\x -> Var (FName x l)) (lookup i ixs)
  open _ c@(Const (FName _ _)) = c
  open ixs c@(Const (BName i l)) =
     maybe c (\x -> Const (FName x l)) (lookup i ixs)
  open _ op@Op{} = op
  open ixs (Chain ch) = Chain (open ixs ch)
  open ixs (App e0 e1 l) = App (open ixs e0) (open ixs e1) l
  open ixs (Lam x e l) = Lam x (open ixs' e) l
     where ixs' = map (first (1+)) ixs
  open ixs (Tuple es) = Tuple (map (open ixs) es)
  open ixs (OutT i e) = OutT i (open ixs e)
  open ixs (Quant op xs ran body l) =
     Quant (open ixs op) xs (open ixs' ran) (open ixs' body) l
    where ixs' = map (first (length xs +)) ixs
  open ixs (ArrIdx arr idx l) =
     ArrIdx (open ixs arr) (open ixs idx) l
  open ixs (ArrUpd arr idx val l) =
     ArrUpd (open ixs arr) (open ixs idx) (open ixs val) l
  open ixs (Case m cls l) =
     Case (open ixs m) (map (open ixs) cls) l
  open _ h@EHole{} = h

instance LocallyNameless Chain where

  close xis (Pure e l) = Pure (close xis e) l
  close xis (More ch op e l) = More (close xis ch) op (close xis e) l

  open ixs (Pure e l) = Pure (open ixs e) l
  open ixs (More ch op e l) = More (open ixs ch) op (open ixs e) l

instance LocallyNameless CaseClause where

  close xis (CaseClause ptn e) = CaseClause ptn (close xis' e)
    where xs = extractBinders ptn
          xis' = map (second (length xs +)) xis

  open ixs (CaseClause ptn e) = CaseClause ptn (open ixs' e)
    where xs = extractBinders ptn
          ixs' = map (first (length xs +)) ixs

extractBinders :: Pattern -> [Text]
extractBinders (PattLit _) = []
extractBinders (PattBinder x) = [x]
extractBinders (PattWildcard _) = []
extractBinders (PattTuple xs) = xs >>= extractBinders
extractBinders (PattConstructor _ xs) = xs >>= extractBinders
