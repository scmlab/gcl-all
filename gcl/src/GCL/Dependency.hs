{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Dependency(resolveDependency) where

import Data.Graph (SCC (..), stronglyConnCompR)
import Data.Map (Map, insert, insertWith, lookup, elems, traverseWithKey, member, lookupIndex, keys)
import Data.Set (Set, toList, singleton, union)
import Control.Monad.State.Lazy (State, get, modify)
import Syntax.Abstract as A
import Syntax.Common as C
import Control.Applicative ((<|>))
import GCL.Type (TypeError (NotInScope, DuplicatedIdentifiers))
import Control.Monad (foldM)
import Data.Text (Text, intercalate)
import qualified Data.Text as Text
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError(throwError))
import Debug.Trace (traceM)
import GCL.Common (Free(freeVars))

{-
  Dependency resolution returns topological-sorted results of dependencies for type and 
  function definitions, type definitions are guaranteed to be sorted before functions.

  Dependency resolution is constructed in the following steps:
  1. Traverse within AST and construct an unresolved dependency map
  1.1 if the relation exists but the dependant is not yet resolved, register it first
      but validate its existence upon finishing construction of map
  1.2 otherwise, if the relation exists and the dependant is resolved, register it.

  2. Validate the dependency map, if relation exists but the dependant does not exist,
     then report as an `NotInScope` error. This step guarantees later dependant's
     type not to be wrapped within `Maybe`.

  After (2), the nodes are namely `ResolvedDepNode`.

  3. Construct the SCCs from `[ResolvedDepNode]`, revserse the result sequence, then
     finally trasnform into `[[Definition]]`.
-}

-- | DepNode consists of an instance, a key represents the dependant,
-- and a collection of keys represents the dependencies that depends 
-- on the dependant.
--
-- The structure is required by `Data.Graph.stronglyConnCompR`.
type DepNode val t = (val, C.Name, t C.Name)
type DepMap node = Map C.Name node

type UnresolvedDepNode = DepNode (Maybe A.Definition) Set
type UnresolvedDepMap = DepMap UnresolvedDepNode

type ResolvedDepNode = DepNode A.Definition []
type ResolvedDepMap = DepMap ResolvedDepNode

-- | Maps from TypeDefnCtor's name to the TypeDefn's name
-- e.g.
--
-- data A = B | C
-- 
-- gives [("B", "A"), ("C", "A")]
-- 
-- This is used to mask out constructors from function dependency.
type TypeDefinitions = Map C.Name C.Name

type DepMonad = ExceptT TypeError (State TypeDefinitions)

resolveDependency :: A.Program -> DepMonad [[A.Definition]]
resolveDependency program = do
  unresolvedDeps <- resolveProgram program
  resolvedDeps <- mapM validateDependency unresolvedDeps
  let sccs = concatMap toTopSortedSCC resolvedDeps
  let depSequence = map degradeSCCNode sccs
  traceM $ show $ map showDependency depSequence
  traceM $ renderDepGraph sccs
  return depSequence
  where
    toTopSortedSCC :: ResolvedDepMap -> [SCC ResolvedDepNode]
    toTopSortedSCC = reverse . stronglyConnCompR . elems

resolveProgram :: A.Program -> DepMonad [UnresolvedDepMap]
resolveProgram (A.Program defs _ _ _ _) = do
  -- Do this in 2 pass, as TypeDefnCtor must be collected first in type resolution pass
  -- then we can distinguish them in term resolution pass.
  typeDeps <- foldM resolveTypeDefinitions mempty defs
  termDeps <- foldM resolveTermDefinitions mempty defs
  return [typeDeps, termDeps]

resolveTypeDefinitions :: UnresolvedDepMap -> A.Definition -> DepMonad UnresolvedDepMap
resolveTypeDefinitions typeDeps def@(A.TypeDefn name _ ctors _) = do
  typeDeps' <- registerDependency name def typeDeps
  foldM resolveTypeDefnCtor typeDeps' ctors
  where
    resolveTypeDefnCtor :: UnresolvedDepMap -> A.TypeDefnCtor -> DepMonad UnresolvedDepMap
    resolveTypeDefnCtor deps' (A.TypeDefnCtor ctorName types) = do
      typeDefs <- get
      -- ChAoS: Should we care about the duplication here?
      -- Checks if type definition constructor has duplicated names
      -- e.g.
      -- data A = B
      -- data C = B
      case Data.Map.lookupIndex ctorName typeDefs of
        Just idx -> do
          let previousCtorName = Data.Map.keys typeDefs !! idx
          throwError $ DuplicatedIdentifiers [previousCtorName, ctorName]
        Nothing ->
          modify $ Data.Map.insert ctorName name
      foldM (resolveType name) deps' types
resolveTypeDefinitions typeDeps _ = return typeDeps

resolveTermDefinitions :: UnresolvedDepMap -> A.Definition -> DepMonad UnresolvedDepMap
resolveTermDefinitions termDeps def@(A.ValDefn name _ clauses) = do
  termDeps' <- registerDependency name def termDeps
  resolveExpr termDeps' clauses
  where
    resolveExpr :: UnresolvedDepMap -> A.Expr -> DepMonad UnresolvedDepMap
    resolveExpr termDeps' expr = do
      let vars = freeVars expr
      typeDefs <- get
      foldM (\deps' var ->
        if Data.Map.member var typeDefs then
          return deps'
        else
          return $ addDependency var name deps'
        ) termDeps' vars
resolveTermDefinitions termDeps _ = return termDeps

resolveType :: C.Name -> UnresolvedDepMap -> A.Type -> DepMonad UnresolvedDepMap
resolveType name deps (A.TArray _ typ _) =
  resolveType name deps typ
resolveType name deps (A.TFunc typA typB _) =
  foldM (resolveType name) deps [typA, typB]
resolveType name deps (A.TData dataName _) =
  return $ addDependency dataName name deps
resolveType name deps (A.TApp typA typB _) =
  foldM (resolveType name) deps [typA, typB]
resolveType _ deps _ = return deps

-- | Registers a dependency graph node to the map, definition may be
-- `Nothing` if definition syntax is not yet identified first before
-- usage.
-- If the provided definition is `Just` and the existed definition in
-- the map is `Nothing`, then replace it with provided one. 
registerDependency :: C.Name -> A.Definition -> UnresolvedDepMap -> DepMonad UnresolvedDepMap
registerDependency name def deps = do
  -- ChAoS: Later accommodate to the refactored definitions
  case (Data.Map.lookup name deps, def) of
    (Just (Just def'@(A.ValDefn {}), _, _), A.ValDefn {}) ->
      reportDuplicate (definitionToName def') name
    (Just (Just def'@(A.TypeDefn {}), _, _), A.TypeDefn {}) ->
      reportDuplicate (definitionToName def') name
    _ ->
      return $ insertWith (\(newDef, _, _) (oldDef, depName, deps') -> (oldDef <|> newDef, depName, deps')) name (pure def, name, mempty) deps
  where
    reportDuplicate :: C.Name -> C.Name -> DepMonad a
    reportDuplicate previous current =
      throwError $ DuplicatedIdentifiers [previous, current]

addDependency :: C.Name -> C.Name -> UnresolvedDepMap -> UnresolvedDepMap
addDependency dependant dependency =
  insertWith (\(_, _, newDeps) (def, depName, oldDeps) ->
    (def, depName, Data.Set.union newDeps oldDeps)) dependant (Nothing, dependant, Data.Set.singleton dependency)

validateDependency :: UnresolvedDepMap -> DepMonad ResolvedDepMap
validateDependency = traverseWithKey validateEntry
  where
    validateEntry :: C.Name -> UnresolvedDepNode -> DepMonad ResolvedDepNode
    validateEntry name (Nothing, _, _) = throwError $ NotInScope name
    validateEntry _ (Just def, name, deps') = return (def, name, Data.Set.toList deps')

{-
  Graph node transformation
-}

degradeSCCNode :: SCC ResolvedDepNode -> [A.Definition]
degradeSCCNode (AcyclicSCC (def, _, _)) = [def]
degradeSCCNode (CyclicSCC defs) = map (\(def, _, _) -> def) defs

showDependency :: [A.Definition] -> Text
showDependency [] = error "should not be empty"
showDependency [def] =
  C.nameToText $ definitionToName def
showDependency defs =
  intercalate (Text.pack " <-> ") $ map (C.nameToText . definitionToName) defs

{-
  Graphviz Rendering
-}

renderDepGraph :: [SCC ResolvedDepNode] -> String
renderDepGraph nodes =
  "digraph DependencyGraph {\n" <>
  "  node [shape=box];\n" <>
  concatMap formatNode nodes <>
  "}"

formatNode :: SCC ResolvedDepNode -> String
formatNode (AcyclicSCC (_, C.Name name _, deps)) =
  let nodeName = show name
      label = nodeName <> "[style=filled, fillcolor=lightblue];\n"
  in "  " <> label <> concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps
formatNode (CyclicSCC defs) =
  let (_, name, _) = head defs
      nodeName = Text.unpack $ nameToText name
      labels = concatMap formatNode'' defs
  in "  subgraph cluster" <> nodeName <> "{\n" <>
     labels <>
     "  }\n"
  where
    formatNode'' (_, C.Name name _, deps) =
      let nodeName = show name
          label = nodeName <> "[style=filled, fillcolor=lightblue];\n"
      in
        "    " <> label <> concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps

definitionToName :: A.Definition -> C.Name
definitionToName (A.TypeDefn name _ _ _) = name
definitionToName (A.ValDefn name _ _) = name
