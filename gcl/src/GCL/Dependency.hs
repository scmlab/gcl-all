module GCL.Dependency (evalDependencyResolution, GCL.Dependency.Program (Program)) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM_)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (evalState)
import Control.Monad.State.Lazy (State, get, modify)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Bifunctor (Bifunctor (..))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Map (Map, elemAt, elems, insert, insertWith, lookup, lookupIndex, member, traverseWithKey)
import Data.Set (Set, elemAt, insert, lookupIndex, member, singleton, toList, union)
import GCL.Common (Free (freeVars))
import GCL.Range (Range)
import GCL.Type (TypeError (DuplicatedIdentifiers, NotInScope))
import Syntax.Abstract as A
import Syntax.Common as C

{-
  Dependency resolution returns topological-sorted results of dependencies for type and
  function definitions, type definitions are guaranteed to be sorted before functions.

  Dependency resolution is constructed in the following steps:
  1. Traverse within AST and construct an unresolved dependency map in 2 passes, which
     are "type phase" and "term phases".
  1.1 if the relation exists but the dependant is not yet resolved, register it first
      but validate its existence upon finishing construction of map.
  1.2 otherwise, if the relation exists and the dependant is resolved, register it.

  2. Validate the dependency map, if relation exists but the dependant does not exist,
     then report as an `NotInScope` error. This step guarantees later dependant's
     type not to be wrapped within `Maybe`.

  After (2), the nodes are namely `ResolvedDepNode`.

  3. Construct the SCCs from `[ResolvedDepNode]`, revserse the result sequence, then
     finally trasnform into `SCC Definition` then construct into an intermediate
     `Program` data for later type checking.

  This pass is responsible for checking:
  1. Duplications of Declarations
  2. Duplications of Type Definition Constructor
  3. Duplications of Definitions
  4. Validity of free variables in term, whether referring to declarations, type
     definition constructor, or other definitions
-}

data Program
  = Program
      [SCC A.Definition] -- definitions (the functional language part)
      [Declaration] -- constant and variable declarations
      [Expr] -- global properties
      [Stmt] -- main program
      (Maybe Range)
  deriving (Eq, Show)

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

-- | Maps declarations' names into a set
type DeclarationMap = Set C.Name

type DepMonad = ExceptT TypeError (State (TypeDefinitions, DeclarationMap))

evalDependencyResolution :: A.Program -> Either TypeError GCL.Dependency.Program
evalDependencyResolution abstract = evalState (runExceptT (resolveDependency abstract)) mempty

resolveDependency :: A.Program -> DepMonad GCL.Dependency.Program
resolveDependency program@(A.Program _ decls exprs stmts range) = do
  forM_ decls checkDeclDuplications
  unresolvedDeps <- resolveProgram program
  resolvedDeps <- mapM validateDependency unresolvedDeps
  let sccs = concatMap toTopSortedSCC resolvedDeps
  return $ GCL.Dependency.Program sccs decls exprs stmts range
  where
    toTopSortedSCC :: ResolvedDepMap -> [SCC A.Definition]
    toTopSortedSCC = reverse . stronglyConnComp . elems

    checkDeclDuplications :: A.Declaration -> DepMonad ()
    checkDeclDuplications (A.ConstDecl names _ _ _) = checkDeclDuplication names
    checkDeclDuplications (A.VarDecl names _ _ _) = checkDeclDuplication names

    checkDeclDuplication :: [C.Name] -> DepMonad ()
    checkDeclDuplication names = do
      (_, declMap) <- get
      declMap' <-
        foldM
          ( \declMap' name -> do
              case Data.Set.lookupIndex name declMap' of
                Just idx -> do
                  let originalName = Data.Set.elemAt idx declMap'
                  throwError $ DuplicatedIdentifiers [originalName, name]
                Nothing -> return $ Data.Set.insert name declMap'
          )
          declMap
          names
      modify $ second $ const declMap'

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
      (typeDefs, _) <- get
      -- Checks if type definition constructor has duplicated names
      -- e.g.
      -- data A = B
      -- data C = B
      case Data.Map.lookupIndex ctorName typeDefs of
        Just idx -> do
          let previousCtorName = snd $ Data.Map.elemAt idx typeDefs
          throwError $ DuplicatedIdentifiers [previousCtorName, ctorName]
        Nothing ->
          modify $ first $ Data.Map.insert ctorName name
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
      (typeDefs, decls) <- get
      foldM
        ( \deps' var ->
            if Data.Map.member var typeDefs || Data.Set.member var decls
              then
                return deps'
              else
                return $ addDependency var name deps'
        )
        termDeps'
        vars
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

-- | Registers a dependency graph node to the map.
registerDependency :: C.Name -> A.Definition -> UnresolvedDepMap -> DepMonad UnresolvedDepMap
registerDependency name def deps = do
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
  insertWith
    ( \(_, _, newDeps) (def, depName, oldDeps) ->
        (def, depName, Data.Set.union newDeps oldDeps)
    )
    dependant
    (Nothing, dependant, Data.Set.singleton dependency)

validateDependency :: UnresolvedDepMap -> DepMonad ResolvedDepMap
validateDependency = traverseWithKey validateEntry
  where
    validateEntry :: C.Name -> UnresolvedDepNode -> DepMonad ResolvedDepNode
    validateEntry name (Nothing, _, _) = throwError $ NotInScope name
    validateEntry _ (Just def, name, deps') = return (def, name, Data.Set.toList deps')
