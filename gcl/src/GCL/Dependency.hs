{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module GCL.Dependency where

import Data.Graph (SCC (AcyclicSCC, CyclicSCC), stronglyConnCompR, graphFromEdges', topSort)
import Data.Map (Map, insert, insertWith, lookup, update, elems, (!))
import Data.Set (Set, empty, insert, toList)
import Control.Monad.State.Lazy (State, get, modify, put)
import Syntax.Abstract as A
import Syntax.Common as C
import Data.Foldable (forM_, traverse_)
import Control.Applicative ((<|>))
import Debug.Trace (traceM, trace)
import GCL.Common (Counterous (..))
import Data.Bifunctor (Bifunctor(..))
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import qualified Data.Text as Text

-- | A dependency graph node is defined as below. The definition
-- | may not exists at first if an usage encountered before the
-- | actual definition, if later definition syntax is encountered,
-- | then the instance of definition is updated.
type DepGraphNode = (Maybe A.Definition, C.Name, Set C.Name)

type TypeDefinitions = Map C.Name C.Name

type SecondaryIdMap = Map C.Name Int

type DepMonad = State (Map C.Name DepGraphNode, TypeDefinitions, SecondaryIdMap, Int)

instance Counterous DepMonad where
  countUp = do
    (deps, defns, smap, i) <- get
    _ <- put (deps, defns, smap, succ i)
    return i

resolveDependency :: A.Program -> DepMonad [DependencyNode]
resolveDependency program = do
  resolveProgram program
  (deps, _, _, _) <- get
  let scc = stronglyConnCompR $ map normalizeNode (elems deps)
  mapSCCIds scc
  secondaryVertexes <- convertToSecondary scc
  let (graph, nodeMapper) = graphFromEdges' secondaryVertexes
  let order = topSort graph
  let !dependency = map nodeMapper order
  -- error $ renderDepGraph' dependency
  return $ degradeNode dependency

resolveProgram :: A.Program -> DepMonad ()
resolveProgram (A.Program defs _ _ _ _) = do
  forM_ defs resolveDefinition

resolveDefinition :: A.Definition -> DepMonad ()
resolveDefinition def@(A.TypeDefn name _ ctors _) = do
  registerDependency name (Just def)
  forM_ ctors resolveTypeDefnCtor
  where
    resolveTypeDefnCtor :: A.TypeDefnCtor -> DepMonad ()
    resolveTypeDefnCtor (A.TypeDefnCtor ctorName types) = do
      forM_ types (\c -> do
        modify (\(deps, defns, smap, id') -> (deps, Data.Map.insert ctorName name defns, smap, id'))
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
  (_, defns, _, _) <- get
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
  modify (\(deps, defns, smap, id') ->
    (insertWith (\(newDef, _, _) (oldDef, depName, deps') -> (oldDef <|> newDef, depName, deps')) name (def, name, Data.Set.empty) deps, defns, smap, id'))

addDependency :: C.Name -> C.Name -> DepMonad ()
addDependency dependant dependency = do
  -- Register dependant as if it hasn't been registered
  registerDependency dependant Nothing
  modify (\(deps, defns, smap, id') ->
    (update (Just . second (Data.Set.insert dependency)) dependant deps, defns, smap, id')
    )

addDependency' :: C.Name -> C.Name -> DepMonad ()
addDependency' = flip addDependency

-- |===========================|
-- | Graph node transformation |
-- |===========================|

type DepGraphNode' = (Maybe A.Definition, C.Name, [C.Name])

normalizeNode :: DepGraphNode -> DepGraphNode'
normalizeNode (def, defName, edges) = (def, defName, Data.Set.toList edges)

type DepGraphSecondaryNode = (NonEmpty DepGraphNode', Int, [Int])

convertToSecondary :: [SCC DepGraphNode'] -> DepMonad [DepGraphSecondaryNode]
convertToSecondary (AcyclicSCC node : ns) = do
  (_, _, idmap, _) <- get
  let (_, name, names) = node
  ns' <- convertToSecondary ns
  return $ (node :| [], idmap ! name, map (idmap !) names) : ns'
convertToSecondary (CyclicSCC nodes : ns) = do
  (_, _, idmap, _) <- get
  ns' <- convertToSecondary ns
  let (_, name, _) = head nodes
  return $ (fromList nodes, idmap ! name, map (\(_, name', _) -> idmap ! name') nodes) : ns'
convertToSecondary [] = return []

mapSCCIds :: [SCC DepGraphNode'] -> DepMonad ()
mapSCCIds (AcyclicSCC node : ns) = do
  let (_, name, _) = node
  nodeId <- countUp
  modify (\(deps, defns, idmap, i) -> (deps, defns, Data.Map.insert name nodeId idmap, i))
  mapSCCIds ns
mapSCCIds (CyclicSCC nodes : ns) = do
  nodeId <- countUp
  forM_ nodes (\node -> do
      let (_, name, _) = node
      modify (\(deps, defns, idmap, i) -> (deps, defns, Data.Map.insert name nodeId idmap, i))
      return ()
    )
  mapSCCIds ns
mapSCCIds [] = return ()

-- TODO: ChAoS: Maybe should not appear here, report when resolution is done
data DependencyNode
  = Acyclic (Maybe A.Definition)
  | Cyclic [Maybe A.Definition]
  deriving (Eq, Show)

degradeNode :: [DepGraphSecondaryNode] -> [DependencyNode]
degradeNode (((node, _, _) :| [], _, _) : ns) =
  Acyclic node : degradeNode ns
degradeNode ((nodes, _, _) : ns) =
  let nodes' = Data.List.NonEmpty.toList nodes
  in Cyclic (map (\(n, _, _) -> n) nodes') : degradeNode ns
degradeNode [] = []

showDependency :: [DependencyNode] -> IO ()
showDependency (Acyclic (Just definition) : ns) = do
  print $ definitionToName definition
  showDependency ns
showDependency (Cyclic defns : ns) = do
  print $ map (\case 
    Just node -> show $ definitionToName node
    Nothing -> "") defns
  showDependency ns
showDependency _ = pure ()

-- |====================|
-- | Graphviz Rendering |
-- |====================|

renderDepGraph :: DepMonad String
renderDepGraph = do
    (nodesMap, _, _, _) <- get
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

renderDepGraph' :: [DepGraphSecondaryNode] -> String
renderDepGraph' nodes =
  "digraph DependencyGraph {\n" <>
  "  node [shape=box];\n" <>
  concatMap formatNode' nodes <>
  "}"

formatNode' :: DepGraphSecondaryNode -> String
formatNode' ((defn, C.Name name _, deps) :| [], _, _) =
  let nodeName = show name
      label = case defn of
        Just _ -> nodeName <> "[style=filled, fillcolor=lightblue];\n"
        Nothing -> nodeName <> " [style=dashed];\n"
  in "  " <> label <> concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps
formatNode' (nodes, nodeId, _) =
  let nodeName = show nodeId
      nodes' = Data.List.NonEmpty.toList nodes
      labels = concatMap formatNode'' nodes'
  in "  subgraph cluster" <> nodeName <> "{\n" <>
     labels <>
     "  }\n"
  where
    formatNode'' (defn, C.Name name _, deps) =
      let nodeName = show name
          label = case defn of
            Just _ -> nodeName <> "[style=filled, fillcolor=lightblue];\n"
            Nothing -> nodeName <> " [style=dashed];\n"
      in "    " <> label <> concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps

definitionToName :: A.Definition -> C.Name
definitionToName (A.TypeDefn name _ _ _) = name
definitionToName (A.FuncDefnSig name _ _ _) = name
definitionToName (A.FuncDefn name _) = name
