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
import Control.Monad (foldM, when)
import Data.Text (Text, intercalate)
import qualified Data.Text as Text
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError(throwError))
import Debug.Trace (traceM)
import GCL.Common (Free(freeVars))
import Data.Bitraversable (bimapM)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Bifoldable (Bifoldable(bifoldMap, bifold))

-- |=========================================================================================
-- | Dependency resolution returns topological-sorted results of dependencies for type and 
-- | function definitions, type definitions are guaranteed to be sorted before functions.
-- |
-- | Dependency resolution is constructed in the following steps:
-- | 1. Traverse within AST and construct an unresolved dependency map
-- | 1.1 if the relation exists but the dependant is not yet resolved, register it first
-- |     but validate its existence upon finishing construction of map
-- | 1.2 otherwise, if the relation exists and the dependant is resolved, register it.
-- |
-- | 2. Validate the dependency map, if relation exists but the dependant does not exist,
-- |    then report as an `NotInScope` error. This step guarantees later dependant's
-- |    type not to be wrapped within `Maybe`.
-- |
-- | After (2), the nodes are namely `ResolvedDepNode`.
-- |
-- | 3. Construct the SCCs from `[ResolvedDepNode]`, revserse the result sequence, then
-- |    finally trasnform into `[[Definition]]`.
-- |=========================================================================================

-- | DepNode consists of an instance, a key represents the dependant,
-- | and a collection of keys represents the dependencies that depends 
-- | on the dependant.
type DepNode val t = (val, C.Name, t C.Name)
type DepMap node = Map C.Name node

type UnresolvedDepNode = DepNode (Maybe A.Definition) Set
type UnresolvedDepMap = DepMap UnresolvedDepNode

type ResolvedDepNode = DepNode A.Definition []
type ResolvedDepMap = DepMap ResolvedDepNode

-- | Maps from TypeDefnCtor's name to the TypeDefn's name
-- | e.g.
-- | data A = B | C
-- | gives [("B", "A"), ("C", "A")]
-- | 
-- | This is used to mask out constructors from function dependency.
type TypeDefinitions = Map C.Name C.Name

type DepMonad = ExceptT TypeError (State TypeDefinitions)

resolveDependency :: A.Program -> DepMonad [[A.Definition]]
resolveDependency program = do
  unresolvedDeps <- resolveProgram program
  resolvedDeps <- bimapM validateDependency validateDependency unresolvedDeps
  let sccs = bimap toTopSortedSCC toTopSortedSCC resolvedDeps
  let result = bifoldMap (map degradeSCCNode) (map degradeSCCNode) sccs
  traceM $ show $ map showDependency result
  traceM $ renderDepGraph $ bifold sccs
  return result
  where
    toTopSortedSCC :: ResolvedDepMap -> [SCC ResolvedDepNode]
    toTopSortedSCC = reverse . stronglyConnCompR . elems

resolveProgram :: A.Program -> DepMonad (UnresolvedDepMap, UnresolvedDepMap)
resolveProgram (A.Program defs _ _ _ _) =
  foldM resolveDefinition mempty defs

resolveDefinition :: (UnresolvedDepMap, UnresolvedDepMap) -> A.Definition -> DepMonad (UnresolvedDepMap, UnresolvedDepMap)
resolveDefinition (typeDeps, funcDeps) def@(A.TypeDefn name _ ctors _) = do
  typeDeps' <- registerDependency name def typeDeps
  typeDeps'' <- foldM resolveTypeDefnCtor typeDeps' ctors
  return (typeDeps'', funcDeps)
  where
    resolveTypeDefnCtor :: UnresolvedDepMap -> A.TypeDefnCtor -> DepMonad UnresolvedDepMap
    resolveTypeDefnCtor deps' (A.TypeDefnCtor ctorName types) = do
      typeDefs <- get
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
resolveDefinition (typeDeps, funcDeps) def@(A.FuncDefnSig name _ expr _) = do
  funcDeps' <- registerDependency name def funcDeps
  funcDeps'' <- foldM (resolveExpr name) funcDeps' expr
  return (typeDeps, funcDeps'')
resolveDefinition (typeDeps, funcDeps) def@(A.FuncDefn name expr) = do
  funcDeps' <- registerDependency name def funcDeps
  funcDeps'' <- resolveExpr name funcDeps' expr
  return (typeDeps, funcDeps'')

resolveExpr :: C.Name -> UnresolvedDepMap -> A.Expr -> DepMonad UnresolvedDepMap
resolveExpr name deps expr = do
  let vars = freeVars expr
  typeDefs <- get
  -- If this free variable is a constructor, then ignore it, otherwise,
  -- consider this as a function and add dependency to its entry
  foldM (\deps' var ->
    if Data.Map.member var typeDefs then
      return deps'
    else
      return $ addDependency var name deps'
    ) deps vars

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
-- | `Nothing` if definition syntax is not yet identified first before
-- | usage.
-- | If the provided definition is `Just` and the existed definition in
-- | the map is `Nothing`, then replace it with provided one. 
registerDependency :: C.Name -> A.Definition -> UnresolvedDepMap -> DepMonad UnresolvedDepMap
registerDependency name def deps = do
  -- ChAoS: Later accommodate to the refactored definitions
  case (Data.Map.lookup name deps, def) of
    (Just (Just def'@(A.FuncDefnSig {}), _, _), A.FuncDefnSig {}) ->
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
addDependency dependant dependency = do
  insertWith (\(_, _, newDeps) (def, depName, oldDeps) ->
    (def, depName, Data.Set.union newDeps oldDeps)) dependant (Nothing, dependant, Data.Set.singleton dependency)

validateDependency :: UnresolvedDepMap -> DepMonad ResolvedDepMap
validateDependency deps = do
  traverseWithKey validateEntry deps
  where
    validateEntry :: C.Name -> UnresolvedDepNode -> DepMonad ResolvedDepNode
    validateEntry name (Nothing, _, _) = throwError $ NotInScope name
    validateEntry _ (Just def, name, deps') = return (def, name, Data.Set.toList deps')

-- |===========================|
-- | Graph node transformation |
-- |===========================|

degradeSCCNode :: SCC ResolvedDepNode -> [A.Definition]
degradeSCCNode (AcyclicSCC (def, _, _)) = [def]
degradeSCCNode (CyclicSCC defs) = map (\(def, _, _) -> def) defs

showDependency :: [A.Definition] -> Text
showDependency [] = error "should not be empty"
showDependency [def] =
  C.nameToText $ definitionToName def
showDependency defs =
  intercalate (Text.pack " <-> ") $ map (C.nameToText . definitionToName) defs

-- |====================|
-- | Graphviz Rendering |
-- |====================|
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
definitionToName (A.FuncDefnSig name _ _ _) = name
definitionToName (A.FuncDefn name _) = name
