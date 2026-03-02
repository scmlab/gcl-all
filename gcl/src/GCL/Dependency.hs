{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Dependency where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Graph (Graph, SCC)
import Data.Map (Map, insert, insertWith, lookup, update, elems)
import Data.Set (Set, empty, insert)
import Control.Monad.State.Lazy (State, get, modify)
import Syntax.Abstract as A
import Syntax.Common as C
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (forM_, traverse_)
import Control.Applicative ((<|>))
import Debug.Trace (traceM)

data DepNode a
  = AcylicNode a
  | CylicNode [a]

-- | A dependency graph node is defined as below. The definition
-- | may not exists at first if an usage encountered before the
-- | actual definition, if later definition syntax is encountered,
-- | then the instance of definition is updated.
type DepGraphNode = (Maybe A.Definition, C.Name, Set C.Name)

type TypeDefinitions = Map C.Name C.Name

type DepMonad = State (Map C.Name DepGraphNode, TypeDefinitions)

resolveDependency :: A.Program -> DepMonad (Map C.Name DepGraphNode)
resolveDependency program = do
  _ <- resolveProgram program
  (deps, _) <- get
  return deps

resolveProgram :: A.Program -> DepMonad ()
resolveProgram (A.Program defs _ _ _ _) = do
  forM_ defs resolveDefinition
  renderDepGraph >>= traceM
  return ()

resolveDefinition :: A.Definition -> DepMonad ()
resolveDefinition def@(A.TypeDefn name _ ctors _) = do
  registerDependency name (Just def)
  forM_ ctors resolveTypeDefnCtor
  where
    resolveTypeDefnCtor :: A.TypeDefnCtor -> DepMonad ()
    resolveTypeDefnCtor (A.TypeDefnCtor ctorName types) = do
      forM_ types (\c -> do
        modify $ second $ Data.Map.insert ctorName name
        resolveType name c
        )
resolveDefinition def@(A.FuncDefnSig name typ expr _) = do
  registerDependency name (Just def)
  resolveType name typ
  traverse_ (resolveExpr name) expr
resolveDefinition def@(A.FuncDefn name expr) = do
  registerDependency name (Just def)
  resolveExpr name expr

resolveExpr :: C.Name -> A.Expr -> DepMonad ()
resolveExpr name (A.Chain chain) = resolveChain chain
  where
    resolveChain :: A.Chain -> DepMonad ()
    resolveChain (Pure expr _) =
      resolveExpr name expr
    resolveChain (More chain' _ expr _) = do
      resolveChain chain'
      resolveExpr name expr
resolveExpr name (A.App exprA exprB _) = do
  -- ChAoS: Is this correct way to identify function invocation?
  case exprA of
    A.Var name' _ -> addDependency name' name
    _ -> return ()
  resolveExpr name exprA
  resolveExpr name exprB
resolveExpr name (A.Lam _ expr _) =
  resolveExpr name expr
resolveExpr name (A.Func funcName clauses _) =
  -- ChAoS: Include this definition later
  -- registerDependency funcName 
  forM_ clauses resolveFuncClause
  where
    resolveFuncClause :: A.FuncClause -> DepMonad ()
    resolveFuncClause (A.FuncClause pats expr) = do
      forM_ pats (resolvePattern name)
      resolveExpr name expr
resolveExpr name (A.Tuple exprs) =
  forM_ exprs (resolveExpr name)
resolveExpr name (A.Quant exprA _ exprB exprC _) = do
  resolveExpr name exprA
  resolveExpr name exprB
  resolveExpr name exprC
resolveExpr name (A.ArrIdx exprA exprB _) = do
  resolveExpr name exprA
  resolveExpr name exprB
resolveExpr name (A.ArrUpd exprA exprB exprC _) = do
  resolveExpr name exprA
  resolveExpr name exprB
  resolveExpr name exprC
resolveExpr name (A.Case expr clauses _) = do
  resolveExpr name expr
  forM_ clauses resolveCaseClause
  where
    resolveCaseClause :: A.CaseClause -> DepMonad ()
    resolveCaseClause (A.CaseClause pat expr') = do
      resolvePattern name pat
      resolveExpr name expr'
resolveExpr _ _ = return ()

resolvePattern :: C.Name -> A.Pattern -> DepMonad ()
resolvePattern name (A.PattConstructor ctorName pats) = do
  (_, defns) <- get
  traverse_ (addDependency' name) (Data.Map.lookup ctorName defns)
  forM_ pats (resolvePattern name)
resolvePattern _ _ = return ()

resolveType :: C.Name -> A.Type -> DepMonad ()
resolveType name (A.TArray _ typ _) = resolveType name typ
resolveType name (A.TFunc typA typB _) = do
  resolveType name typA
  resolveType name typB
resolveType name (A.TData dataName _) = do
  addDependency dataName name
resolveType name (A.TApp typA typB _) = do
  resolveType name typA
  resolveType name typB
resolveType _ _ = return ()

-- | Registers a dependency graph node to the map, definition may be
-- | Nothing if definition syntax is not yet identified first before
-- | usage.
-- | If the provided definition is Just and the existed definition in
-- | the map is Nothing, then replace it with provided one. 
registerDependency :: C.Name -> Maybe A.Definition -> DepMonad ()
registerDependency name def =
  modify $ first $ insertWith (\(newDef, _, _) (oldDef, depName, deps) -> (oldDef <|> newDef, depName, deps)) name (def, name, Data.Set.empty)

addDependency :: C.Name -> C.Name -> DepMonad ()
addDependency dependant dependency = do
  -- Register dependant as if it hasn't been registered
  registerDependency dependant Nothing
  modify $ first $ update (Just . second (Data.Set.insert dependency)) dependant

addDependency' :: C.Name -> C.Name -> DepMonad ()
addDependency' = flip addDependency

-- |====================|
-- | Graphviz Rendering |
-- |====================|

renderDepGraph :: DepMonad String
renderDepGraph = do
    (nodesMap, _) <- get
    let nodes = Data.Map.elems nodesMap
    return $ "digraph DependencyGraph {\n"
           <> "  node [shape=box];\n"
           <> concatMap formatNode nodes
           <> "}"

formatNode :: DepGraphNode -> String
formatNode (maybeDef, C.Name name _, deps) =
    let nodeName = show name
        -- Label the node; you might want to add info from A.Definition here
        label = case maybeDef of
            Just _  -> nodeName ++ " [style=filled, fillcolor=lightblue];\n"
            Nothing -> nodeName ++ " [style=dashed];\n"

        -- Create edges for each dependency
        edges = concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps
    in "  " ++ label ++ edges
