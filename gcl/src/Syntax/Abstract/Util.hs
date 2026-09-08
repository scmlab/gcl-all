{-# LANGUAGE OverloadedStrings #-}

module Syntax.Abstract.Util where

import GCL.Range (MaybeRanged (maybeRangeOf), (<--->))
import Syntax.Abstract
import Syntax.Common
  ( Name (..),
    TypeOp (Arrow),
  )

wrapTFunc :: [Type] -> Type -> Type
wrapTFunc [] t = t
wrapTFunc (t : ts) t0 = let t0' = wrapTFunc ts t0 in TApp (TApp (TOp (Arrow Nothing)) t Nothing) t0' (maybeRangeOf t0) -- TODO: What should the loc be?

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [Name] -> Expr -> Expr
wrapLam [] body = body
wrapLam (x : xs) body = let b = wrapLam xs body in Lam x b (maybeRangeOf x <---> maybeRangeOf b)

declaredNames :: [Declaration] -> [Name]
declaredNames decls = concat . map extractNames $ decls
  where
    extractNames (ConstDecl ns _ _ _) = ns
    extractNames (VarDecl ns _ _ _) = ns

baseToName :: TBase -> Name
baseToName TInt = Name "Int" Nothing
baseToName TBool = Name "Bool" Nothing
baseToName TChar = Name "Char" Nothing

nameToVar :: Name -> Expr
nameToVar name = Var name (maybeRangeOf name)
