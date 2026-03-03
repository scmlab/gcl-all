{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GCL.Dependency(resolveDependency, showDependency, DependencyNode) where

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
-- | After (2), the nodes are namely `ResolvedDepNode`.
-- |
-- | 3. Construct the SCCs from `[ResolvedDepNode]`, revserse the result sequence, then
-- |    finally trasnform into `[DependencyNode]`.
-- |=========================================================================================

type DepNode val t = (val, C.Name, t C.Name)
type DepMap node = Map C.Name node

type UnresolvedDepNode = DepNode (Maybe A.Definition) Set
type UnresolvedDepMap = DepMap UnresolvedDepNode

type ResolvedDepMap = DepMap ResolvedDepNode
type ResolvedDepNode = DepNode A.Definition []

data DependencyNode
  = Acyclic A.Definition
  | Cyclic [A.Definition]
  deriving (Eq, Show)

type TypeDefinitions = Map C.Name C.Name

type DepMonad = ExceptT TypeError (State TypeDefinitions)

resolveDependency :: A.Program -> DepMonad [DependencyNode]
resolveDependency program = do
  deps <- resolveProgram program
  resolvedDeps <- validateDependency deps
  let scc = reverse $ stronglyConnCompR $ elems resolvedDeps
  -- error $ renderDepGraph scc
  return $ constructNode scc

resolveProgram :: A.Program -> DepMonad UnresolvedDepMap
resolveProgram (A.Program defs _ _ _ _) =
  foldM resolveDefinition mempty defs

resolveDefinition :: UnresolvedDepMap -> A.Definition -> DepMonad UnresolvedDepMap
resolveDefinition deps def@(A.TypeDefn name _ ctors _) = do
  deps' <- registerDependency name def deps
  foldM resolveTypeDefnCtor deps' ctors
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
        Nothing -> do
          modify $ Data.Map.insert ctorName name
      foldM (resolveType name) deps' types
resolveDefinition deps def@(A.FuncDefnSig name typ expr _) = do
  deps' <- registerDependency name def deps
  deps'' <- resolveType name deps' typ
  foldM (resolveExpr name) deps'' expr
resolveDefinition deps def@(A.FuncDefn name expr) = do
  deps' <- registerDependency name def deps
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
  defns <- get
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
registerDependency :: C.Name -> A.Definition -> UnresolvedDepMap -> DepMonad UnresolvedDepMap
registerDependency name def deps = do
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

constructNode :: [SCC ResolvedDepNode] -> [DependencyNode]
constructNode (AcyclicSCC (def, _, _) : ns) =
  Acyclic def : constructNode ns
constructNode (CyclicSCC defs : ns) =
  Cyclic (map (\(n, _, _) -> n) defs) : constructNode ns
constructNode [] = []

showDependency :: DependencyNode -> Text
showDependency (Acyclic definition) =
  C.nameToText $ definitionToName definition
showDependency (Cyclic defns) =
  intercalate (Text.pack " <-> ") $ map (C.nameToText . definitionToName) defns

showDependency' :: SCC ResolvedDepNode -> Text
showDependency' (AcyclicSCC (definition, _, _)) =
  C.nameToText $ definitionToName definition
showDependency' (CyclicSCC definitions) =
  intercalate (Text.pack " <-> ") $ map (\(def, _, _) -> C.nameToText $ definitionToName def) definitions

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
      in "    " <> label <> concatMap (\(C.Name name' _) -> "  " <> nodeName <> " -> " <> show name' <> ";\n") deps

definitionToName :: A.Definition -> C.Name
definitionToName (A.TypeDefn name _ _ _) = name
definitionToName (A.FuncDefnSig name _ _ _) = name
definitionToName (A.FuncDefn name _) = name
