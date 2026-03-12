{-# LANGUAGE OverloadedStrings #-}

module Syntax.Typed.Util where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GCL.Range (MaybeRanged (..), (<--->))
import Syntax.Abstract.Types (TBase (TBool), Type (..))
import qualified Syntax.Abstract.Types as A
import Syntax.Common (Name (..), nameToText)
import Syntax.Typed

getGuards :: [GdCmd] -> [Expr]
getGuards = fst . unzipGdCmds

unzipGdCmds :: [GdCmd] -> ([Expr], [[Stmt]])
unzipGdCmds = unzip . map (\(GdCmd x y _) -> (x, y))

wrapLam :: [(Name, Type)] -> Expr -> Expr
wrapLam [] body = body
wrapLam ((x, t) : xs) body = let b = wrapLam xs body in Lam x t b (maybeRangeOf x <---> maybeRangeOf b)

declaredNames :: [Declaration] -> [Name]
declaredNames decls = concat . map extractNames $ decls
  where
    extractNames (ConstDecl ns _ _ _) = ns
    extractNames (VarDecl ns _ _ _) = ns

declaredNamesTypes :: [Declaration] -> [(Name, Type)]
declaredNamesTypes decls = concat . map extractNames $ decls
  where
    extractNames (ConstDecl ns t _ _) = [(n, t) | n <- ns]
    extractNames (VarDecl ns t _ _) = [(n, t) | n <- ns]

typeOf :: Expr -> Type
typeOf (Lit _ t _) = t
typeOf (Var _ t _) = t
typeOf (Const _ t _) = t
typeOf (Op _ t) = t
typeOf (Chain ch) = typeOfChain ch
typeOf (App e0 _ _) = case typeOf e0 of
  A.TFunc _ t _ -> t
  _ -> error "left term not having function type in a typed expression"
typeOf (Lam _ t0 e _) = A.TFunc t0 (typeOf e) Nothing
typeOf (Tuple es) = A.TTuple (map typeOf es)
typeOf (OutT i e) = case typeOf e of
  A.TTuple ts -> if i < length ts then ts !! i
                 else error "tuple too short"
  _ -> error "outT applied a non-tuple"
typeOf (Quant _ _ _ body _) = typeOf body
typeOf (ArrIdx arr _ _) = case typeOf arr of
  A.TArray _ t _ -> t
  A.TFunc _ t _ -> t
  _ -> error "indexed term not an array in a typed expression "
typeOf (ArrUpd arr _ _ _) = typeOf arr
typeOf (Case expr _ _) = typeOf expr -- FIXME: This is wrong and acts as a placeholder. Figure out what this should be.
typeOf (Subst e _) = typeOf e
typeOf (EHole _ _ t _) = t

typeOfChain :: Chain -> Type
typeOfChain (Pure e) = typeOf e -- SCM: shouldn't happen?
typeOfChain (More _ _ _ _) = A.TBase TBool Nothing

programToScopeForSubstitution :: Program -> Map Text (Maybe Expr)
programToScopeForSubstitution (Program defns decls _ _ _) =
  Map.mapKeys nameToText $
    foldMap extractDeclaration decls
      <> Map.fromList (extractDefinition defns)
  where
    extractDeclaration :: Declaration -> Map Name (Maybe Expr)
    extractDeclaration (ConstDecl names _ _ _) =
      Map.fromList (zip names (repeat Nothing))
    extractDeclaration (VarDecl names _ _ _) =
      Map.fromList (zip names (repeat Nothing))

    extractDefinition :: [Definition] -> [(Name, Maybe Expr)]
    extractDefinition [] = []
    extractDefinition (TypeDefn _ _ _ _ : ds) =
      extractDefinition ds
    extractDefinition (ValDefn n _ e : ds) =
      (n, Just e) : extractDefinition ds

syntaxSubst :: [Name] -> [Expr] -> Expr -> Expr
syntaxSubst xs es e = Subst e (zip xs es)
