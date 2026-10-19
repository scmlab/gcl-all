{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Syntax.ConstExpr where

import Data.List (partition)
import Data.Maybe (mapMaybe)
import GCL.Common (freeVars)
import Pretty.Typed ()
import Syntax.Abstract.Types (Declaration (..), Expr)
import Syntax.Common.Types (Name)

data DeclType = Const | Var
  deriving (Show)

type Env = [(Name, DeclType)]

pickGlobals :: [Declaration] -> ([Expr], [Expr])
pickGlobals decls =
  partition (isGlobalProp env') (mapMaybe extractAssertion decls)
  where
    -- if each `Declaration` contained one name we probably wouldn't need this
    env' =
      concatMap
        ( \case
            ConstDecl names _ _ _ -> map (,Const) names
            VarDecl names _ _ _ -> map (,Var) names
        )
        decls

    -- An assertion is a global property
    -- if all of its free variables are of CONSTANTS
    isGlobalProp :: Env -> Expr -> Bool
    isGlobalProp env assertion = all (isConst env) (freeVars assertion)

    isConst :: Env -> Name -> Bool
    isConst env name =
      case lookup name env of
        Just Const -> True
        Just Var -> False
        Nothing -> error "should not happen"

    -- Extracts both the assertion and those declared names
    extractAssertion :: Declaration -> Maybe Expr
    extractAssertion (ConstDecl _ _ e _) = e
    extractAssertion (VarDecl _ _ e _) = e
