{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Dependency(resolveDependency, showDependency, DependencyNode) where

import Data.Graph (SCC (AcyclicSCC, CyclicSCC), stronglyConnCompR, graphFromEdges', topSort)
import Data.Map (Map, insert, insertWith, lookup, elems, (!), traverseWithKey)
import Data.Set (Set, toList, singleton, union)
import Control.Monad.State.Lazy (State, get, modify, put)
import Syntax.Abstract as A
import Syntax.Common as C
import Data.Foldable (forM_)
import Control.Applicative ((<|>))
import Debug.Trace (traceM)
import GCL.Common (Counterous (..))
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import GCL.Type (TypeError (NotInScope))
import Control.Monad (foldM)
import Data.Text (Text, intercalate)
import qualified Data.Text as Text

-- |=========================================================================================
-- | Depdendency resolution is constructed in follow steps:
-- | 1. Traverse within AST and construct an unresolved dependency map
-- | 1.1 if the relation exists but the dependant is not yet resolved, register it first
-- |     but validate its existence upon finishing construction of map
-- | 1.2 otherwise, if the relation exists and the dependant is resolved, register it.
-- |
-- | 2. Validate the dependency map, if relation exists but the dependant does not exist,
-- |    then report as an `NotInScope` error. This step guarantees later dependant's
-- |    type not to be wrapped within `Maybe`.
-- |
-- | After (2), the nodes are namely `DepGraphPrimaryNode`.
-- |
-- | 3. Construct the SCCs from `[DepGraphPrimaryNode]`, then map SCCs to an identical IDs
-- |    and store under `DepMonad`.
-- |    
-- | 4. Degrades them into `[DepGraphSecondaryNode]` given their original IDs computed from
-- |    previous step.
-- |
-- | 5. Construct `[DepGraphSecondaryNode]` into a graph then perform topological sort on it.
-- |
-- | 6. Finally, construct `[DependencyNode]` from the order given by topological sort and
-- |    the `[DepGraphSecondaryNode]`.
-- |=========================================================================================

type DepGraphNode val edge t = (val, edge, t edge)

type DepMap val edges = Map C.Name (DepGraphNode val C.Name edges)

type UnresolvedDepMap = DepMap (Maybe A.Definition) Set

type ResolvedDepMap = DepMap A.Definition []

type Result a = Either TypeError a

data DependencyNode
  = Acyclic A.Definition
  | Cyclic [A.Definition]
  deriving (Eq, Show)

type TypeDefinitions = Map C.Name C.Name

type SecondaryIdMap = Map C.Name Int

type DepMonad = State (TypeDefinitions, SecondaryIdMap, Int)

instance Counterous DepMonad where
  countUp = do
    (defns, smap, i) <- get
    _ <- put (defns, smap, succ i)
    return i

resolveDependency :: A.Program -> DepMonad (Result [DependencyNode])
resolveDependency program = do
  deps <- resolveProgram program
  resolvedDeps <- validateDependency deps
  case resolvedDeps of
    Left err -> return $ Left err
    Right resolvedDeps' -> do
      let scc = stronglyConnCompR $ elems resolvedDeps'
      mapSCCIds scc
      secondaryVertexes <- degradeNode scc
      let (graph, nodeMapper) = graphFromEdges' secondaryVertexes
      let order = topSort graph
      let dependency = map nodeMapper order
      -- error $ renderDepGraph' dependency
      return $ pure $ constructNode dependency

resolveProgram :: A.Program -> DepMonad UnresolvedDepMap
resolveProgram (A.Program defs _ _ _ _) =
  foldM resolveDefinition mempty defs

resolveDefinition :: UnresolvedDepMap -> A.Definition -> DepMonad UnresolvedDepMap
resolveDefinition deps def@(A.TypeDefn name _ ctors _) = do
  let deps' = registerDependency name (Just def) deps
  foldM resolveTypeDefnCtor deps' ctors
  where
    resolveTypeDefnCtor :: UnresolvedDepMap -> A.TypeDefnCtor -> DepMonad UnresolvedDepMap
    resolveTypeDefnCtor deps' (A.TypeDefnCtor ctorName types) = do
      foldM (\deps'' typ -> do
        modify (\(defns, smap, id') -> (Data.Map.insert ctorName name defns, smap, id'))
        resolveType name deps'' typ
        ) deps' types
resolveDefinition deps def@(A.FuncDefnSig name typ expr _) = do
  let deps' = registerDependency name (Just def) deps
  deps'' <- resolveType name deps' typ
  foldM (resolveExpr name) deps'' expr
resolveDefinition deps def@(A.FuncDefn name expr) = do
  let deps' = registerDependency name (Just def) deps
  resolveExpr name deps' expr

resolveExpr :: C.Name -> UnresolvedDepMap -> A.Expr -> DepMonad UnresolvedDepMap
resolveExpr name deps (A.Chain chain) = resolveChain chain
  where
    resolveChain :: A.Chain -> DepMonad UnresolvedDepMap
    resolveChain (Pure expr _) =
      resolveExpr name deps expr
    resolveChain (More chain' _ expr _) = do
      deps' <- resolveChain chain'
      resolveExpr name deps' expr
resolveExpr name deps (A.App exprA exprB _) = do
  -- ChAoS: Is this correct way to identify function invocation?
  let deps' = case exprA of
        A.Var name' _ -> addDependency name' name deps
        _ -> deps
  deps'' <- resolveExpr name deps' exprA
  resolveExpr name deps'' exprB
resolveExpr name deps (A.Lam _ expr _) =
  resolveExpr name deps expr
resolveExpr name deps (A.Func funcName clauses _) =
  -- ChAoS: Include this definition later
  -- registerDependency funcName
  foldM resolveFuncClause deps clauses
  where
    resolveFuncClause :: UnresolvedDepMap -> A.FuncClause -> DepMonad UnresolvedDepMap
    resolveFuncClause deps' (A.FuncClause pats expr) = do
      deps'' <- foldM (resolvePattern name) deps' pats
      resolveExpr name deps'' expr
resolveExpr name deps (A.Tuple exprs) =
  foldM (resolveExpr name) deps exprs
resolveExpr name deps (A.Quant exprA _ exprB exprC _) = do
  foldM (resolveExpr name) deps [exprA, exprB, exprC]
resolveExpr name deps (A.ArrIdx exprA exprB _) = do
  foldM (resolveExpr name) deps [exprA, exprB]
resolveExpr name deps (A.ArrUpd exprA exprB exprC _) = do
  foldM (resolveExpr name) deps [exprA, exprB, exprC]
resolveExpr name deps (A.Case expr clauses _) = do
  deps' <- resolveExpr name deps expr
  foldM resolveCaseClause deps' clauses
  where
    resolveCaseClause :: UnresolvedDepMap -> A.CaseClause -> DepMonad UnresolvedDepMap
    resolveCaseClause deps' (A.CaseClause pat expr') = do
      deps'' <- resolvePattern name deps' pat
      resolveExpr name deps'' expr'
resolveExpr _ deps _ = return deps

resolvePattern :: C.Name -> UnresolvedDepMap -> A.Pattern -> DepMonad UnresolvedDepMap
resolvePattern name deps (A.PattConstructor ctorName pats) = do
  (defns, _, _) <- get
  let deps' = case Data.Map.lookup ctorName defns of
        Just dependent -> addDependency dependent name deps
        Nothing -> deps
  foldM (resolvePattern name) deps' pats
resolvePattern _ deps _ = return deps

resolveType :: C.Name -> UnresolvedDepMap -> A.Type -> DepMonad UnresolvedDepMap
resolveType name deps (A.TArray _ typ _) = resolveType name deps typ
resolveType name deps (A.TFunc typA typB _) = do
  foldM (resolveType name) deps [typA, typB]
resolveType name deps (A.TData dataName _) = do
  return $ addDependency dataName name deps
resolveType name deps (A.TApp typA typB _) = do
  foldM (resolveType name) deps [typA, typB]
resolveType _ deps _ = return deps

-- | Registers a dependency graph node to the map, definition may be
-- | `Nothing` if definition syntax is not yet identified first before
-- | usage.
-- | If the provided definition is `Just` and the existed definition in
-- | the map is `Nothing`, then replace it with provided one. 
registerDependency :: C.Name -> Maybe A.Definition -> UnresolvedDepMap -> UnresolvedDepMap
registerDependency name def =
  insertWith (\(newDef, _, _) (oldDef, depName, deps') -> (oldDef <|> newDef, depName, deps')) name (def, name, mempty)

addDependency :: C.Name -> C.Name -> UnresolvedDepMap -> UnresolvedDepMap
addDependency dependant dependency = do
  insertWith (\(_, _, newDeps) (def, depName, oldDeps) ->
    (def, depName, Data.Set.union newDeps oldDeps)) dependant (Nothing, dependant, Data.Set.singleton dependency)

validateDependency :: UnresolvedDepMap -> DepMonad (Result ResolvedDepMap)
validateDependency deps = do
  return $ traverseWithKey validateEntry deps
  where
    validateEntry :: C.Name -> DepGraphNode (Maybe A.Definition) C.Name Set -> Result DepGraphPrimaryNode
    validateEntry name (Nothing, _, _) = Left $ NotInScope name
    validateEntry _ (Just def, name, deps') = pure (def, name, Data.Set.toList deps')

-- |===========================|
-- | Graph node transformation |
-- |===========================|

type DepGraphPrimaryNode = DepGraphNode A.Definition C.Name []

type DepGraphSecondaryNode = DepGraphNode (NonEmpty DepGraphPrimaryNode) Int []

degradeNode :: [SCC DepGraphPrimaryNode] -> DepMonad [DepGraphSecondaryNode]
degradeNode (AcyclicSCC node : ns) = do
  (_, idmap, _) <- get
  let (_, name, names) = node
  ns' <- degradeNode ns
  return $ (node :| [], idmap ! name, map (idmap !) names) : ns'
degradeNode (CyclicSCC nodes : ns) = do
  (_, idmap, _) <- get
  ns' <- degradeNode ns
  let (_, name, _) = head nodes
  return $ (fromList nodes, idmap ! name, map (\(_, name', _) -> idmap ! name') nodes) : ns'
degradeNode [] = return []

mapSCCIds :: [SCC DepGraphPrimaryNode] -> DepMonad ()
mapSCCIds (AcyclicSCC node : ns) = do
  let (_, name, _) = node
  nodeId <- countUp
  modify (\(defns, idmap, i) -> (defns, Data.Map.insert name nodeId idmap, i))
  mapSCCIds ns
mapSCCIds (CyclicSCC nodes : ns) = do
  nodeId <- countUp
  forM_ nodes (\node -> do
      let (_, name, _) = node
      modify (\(defns, idmap, i) -> (defns, Data.Map.insert name nodeId idmap, i))
      return ()
    )
  mapSCCIds ns
mapSCCIds [] = return ()

constructNode :: [DepGraphSecondaryNode] -> [DependencyNode]
constructNode (((node, _, _) :| [], _, _) : ns) =
  Acyclic node : constructNode ns
constructNode ((nodes, _, _) : ns) =
  let nodes' = Data.List.NonEmpty.toList nodes
  in Cyclic (map (\(n, _, _) -> n) nodes') : constructNode ns
constructNode [] = []

showDependency :: DependencyNode -> Text
showDependency (Acyclic definition) =
  C.nameToText $ definitionToName definition
showDependency (Cyclic defns) =
  intercalate (Text.pack " <-> ") $ map (C.nameToText . definitionToName) defns

-- |====================|
-- | Graphviz Rendering |
-- |====================|
renderDepGraph :: [DepGraphSecondaryNode] -> String
renderDepGraph nodes =
  "digraph DependencyGraph {\n" <>
  "  node [shape=box];\n" <>
  concatMap formatNode nodes <>
  "}"

formatNode :: DepGraphSecondaryNode -> String
formatNode ((_, C.Name name _, deps) :| [], _, _) =
  let nodeName = show name
      label = nodeName <> "[style=filled, fillcolor=lightblue];\n"
  in "  " <> label <> concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps
formatNode (nodes, nodeId, _) =
  let nodeName = show nodeId
      nodes' = Data.List.NonEmpty.toList nodes
      labels = concatMap formatNode'' nodes'
  in "  subgraph cluster" <> nodeName <> "{\n" <>
     labels <>
     "  }\n"
  where
    formatNode'' (_, C.Name name _, deps) =
      let nodeName = show name
          label = nodeName <> "[style=filled, fillcolor=lightblue];\n"
      in "    " <> label <> concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps

definitionToName :: A.Definition -> C.Name
definitionToName (A.TypeDefn name _ _ _) = name
definitionToName (A.FuncDefnSig name _ _ _) = name
definitionToName (A.FuncDefn name _) = name
