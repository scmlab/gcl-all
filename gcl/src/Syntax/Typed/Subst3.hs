-- | Definition-aware substitution. Local binders and program globals have
-- separate identities inside the reducer, even when their printed names agree.
-- A reference carries only actual state updates; dependencies live in the
-- definition context rather than identity substitutions in the displayed AST.
-- Pending state tables compose without executing their subjects. Lexical
-- replacements that would be captured are delayed in a fresh table slot,
-- keeping beta/case arguments in the caller's state.
--
-- The internal tree retains Expr headers for types, ranges and source layout.
-- Its children use the shared path order. Export restores ordinary Exprs and
-- alpha-renames local binders that would hide a displayed global or reference.
-- Holes remain opaque, with the same source-Env caveat as Subst2.
module Syntax.Typed.Subst3
  ( DefnEnv,
    Definitions,
    Term (..),
    Scope (..),
    Substitution,
    definitions,
    resolve,
    export,
    unfold,
    substituteLocal,
    substituteState,
    termChildren,
    withTermChildren,
  )
where

import Control.Monad (foldM)
import Data.List ((!?))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GCL.Common (Fresh, freshAvoiding)
import Syntax.Abstract.Types (extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Structure
import Syntax.Typed.Types

type DefnEnv = [(Name, Expr)]

type Substitution = [(Name, Term)]

data Scope = Local | Global deriving (Eq, Show)

data Term
  = Variable Scope Expr
  | Reference Expr Substitution
  | Syntax Expr [Term]
  deriving (Eq, Show)

data Definitions = Definitions
  { bodies :: Map Name Term,
    dependencies :: Map Name (Set Name),
    forbidden :: Set Text
  }

-- Fixed-point sets handle recursive aliases without relying on source ranges.
definitions :: DefnEnv -> Definitions
definitions env = context
  where
    names = Set.fromList (map fst env)
    compiled = Map.fromList [(name, resolveWith names Set.empty body) | (name, body) <- env]
    context = Definitions compiled (fixedPoint Map.empty) avoid
    avoid = Set.map nameToText names <> foldMap (allNames . snd) env
    fixedPoint known =
      let next = Map.map (freeFor (context {dependencies = known}) State) compiled
       in if next == known then known else fixedPoint next

resolve :: Definitions -> Expr -> Term
resolve context = resolveWith (Map.keysSet (bodies context)) Set.empty

resolveWith :: Set Name -> Set Name -> Expr -> Term
resolveWith visible bound expression@(Var name _ _)
  | name `Set.member` bound = Variable Local expression
  | name `Set.member` visible = Reference expression []
  | otherwise = Variable Global expression
resolveWith _ bound expression@(Const name _ _) =
  Variable (if name `Set.member` bound then Local else Global) expression
resolveWith visible bound expression = Syntax expression (zipWith child [0 ..] (pathChildren expression))
  where
    child index body =
      let names = Set.fromList (childBinders expression !! index)
          local = case expression of Subst {} -> Set.empty; _ -> names
       in resolveWith (visible Set.\\ names) (bound <> local) body

data Mode = Lexical | State

-- Only Local occurrences are parameters/pattern variables. State substitution
-- reaches Globals and reference environments, including under a same-named
-- local binder. Replacement values are simultaneous and read the old state.
substituteLocal, substituteState :: (Fresh m) => Definitions -> Substitution -> Term -> m Term
substituteLocal context = checked context Lexical
substituteState context = checked context State

checked :: (Fresh m) => Definitions -> Mode -> Substitution -> Term -> m Term
checked context mode table
  | length table /= Set.size (Set.fromList (map fst table)) = error "Subst3: duplicate substitution keys"
  | otherwise = substitute context mode table

substitute :: (Fresh m) => Definitions -> Mode -> Substitution -> Term -> m Term
substitute _ _ [] term = pure term
substitute _ mode table term@(Variable scope expression)
  | targets mode scope = pure (maybe term id (lookup (atomName expression) table))
  | otherwise = pure term
substitute context mode table (Reference expression environment) = do
  values <- mapM (\(key, value) -> (,) key <$> substitute context mode table value) environment
  let globalsRead = Map.findWithDefault Set.empty (atomName expression) (dependencies context)
      missing = case mode of
        Lexical -> []
        State -> [(key, value) | (key, value) <- table, key `Set.member` globalsRead, key `notElem` map fst environment]
  pure (Reference expression (filter (not . identityEntry) (values <> missing)))
substitute context State table (Syntax (Subst _ assignments) (subject : values)) = do
  -- (e[S])[T] = e[S-values[T], T-keys outside S]. The earlier subject
  -- stays untouched, so executing the composed table is a separate click.
  earlier <- mapM update (zip (map fst assignments) values)
  let remaining = freeFor context State subject Set.\\ Set.fromList (map fst assignments)
  pure (pendingState subject (earlier <> filter ((`Set.member` remaining) . fst) table))
  where
    update (key, value) = (,) key <$> substituteState context table value
substitute context Lexical table (Syntax header@(Subst _ assignments) (subject : values)) = do
  -- A beta/case argument reads the state outside this pending operation.
  -- If inserting it into the subject would let a state key capture it,
  -- defer that insertion as a fresh slot in the same simultaneous table:
  -- (t + x)[x := 5], with t := x, becomes (t' + x)[x := 5, t' := x].
  -- No pending operation is executed, and the incoming x stays outside S.
  let active = [(key, value) | (key, value) <- table, key `Set.member` freeFor context Lexical subject]
      domain = Set.fromList (map fst assignments)
      exposed value = freeFor context State value <> referenceNames value
      clashes = [(key, value) | (key, value) <- active, not (Set.null (exposed value `Set.intersection` domain))]
      avoid =
        forbidden context
          <> allNames header
          <> foldMap termNames (subject : values)
          <> foldMap (termNames . snd) table
          <> Set.fromList (map (nameToText . fst) table)
  renaming <- allocateNames avoid (map fst clashes)
  subject' <- substituteLocal context table (renameLocalsAs Global renaming subject)
  values' <- mapM (substituteLocal context table) values
  let delayed = [(renameName renaming key, value) | (key, value) <- clashes]
  pure (pendingState subject' (zip (map fst assignments) values' <> delayed))
substitute context mode table (Syntax header children) = do
  (header', children') <- prepareScopes context choose header children
  Syntax header' <$> zipWithMChild header' children'
  where
    active names region =
      let keys = foldMap (freeFor context mode) region
       in [(key, value) | (key, value) <- table, key `Set.member` keys, case mode of Lexical -> key `notElem` names; State -> True]
    choose names region = foldMap (freeNames context . snd) (active names region)
    zipWithMChild header' = mapM child . zip [0 ..]
      where
        child (index, body) =
          let names = localBinders header' index
              table' = case mode of Lexical -> filter ((`notElem` names) . fst) table; State -> table
           in substitute context mode table' body

targets :: Mode -> Scope -> Bool
targets Lexical Local = True
targets State Global = True
targets _ _ = False

identityEntry :: (Name, Term) -> Bool
identityEntry (key, Variable Global expression) = key == atomName expression
identityEntry _ = False

unfold :: (Fresh m) => Definitions -> Term -> m (Maybe Term)
unfold context (Reference expression environment) =
  pure (fmap attach (Map.lookup (atomName expression) (bodies context)))
  where
    attach body = case environment of [] -> body; _ -> pendingState body environment
unfold _ _ = pure Nothing

-- Keep actual state updates explicit until their own reduction step. Headers
-- retain source metadata; the resolved children carry the current expressions.
pendingState :: Term -> Substitution -> Term
pendingState subject table =
  Syntax (Subst (termHeader subject) [(key, termHeader value) | (key, value) <- table]) (subject : map snd table)
  where
    termHeader (Variable _ expression) = expression
    termHeader (Reference expression _) = expression
    termHeader (Syntax expression _) = expression

-- Free locals follow lexical scopes. Globals retain declaration identity.
freeFor :: Definitions -> Mode -> Term -> Set Name
freeFor _ mode (Variable scope expression)
  | targets mode scope = Set.singleton (atomName expression)
  | otherwise = Set.empty
freeFor context mode (Reference expression environment) =
  foldMap (freeFor context mode . snd) environment <> hidden
  where
    hidden = case mode of
      Lexical -> Set.empty
      State -> Map.findWithDefault Set.empty (atomName expression) (dependencies context) Set.\\ Set.fromList (map fst environment)
freeFor context mode (Syntax (Subst _ assignments) (subject : values)) =
  remaining <> foldMap (freeFor context mode . snd) relevant
  where
    keys = freeFor context State subject
    relevant = [(key, value) | (key, value) <- zip (map fst assignments) values, key `Set.member` keys]
    remaining = case mode of
      Lexical -> freeFor context mode subject
      State -> keys Set.\\ Set.fromList (map fst assignments)
freeFor context mode (Syntax header children) =
  foldMap child (zip [0 ..] children)
  where
    child (index, body) = case mode of
      Lexical -> freeFor context mode body Set.\\ Set.fromList (localBinders header index)
      State -> freeFor context mode body

freeNames :: Definitions -> Term -> Set Name
freeNames context term = freeFor context Lexical term <> freeFor context State term <> referenceNames term

referenceNames :: Term -> Set Name
referenceNames (Reference expression environment) = Set.singleton (atomName expression) <> foldMap (referenceNames . snd) environment
referenceNames term = foldMap referenceNames (termChildren term)

-- Only names actually displayed can change resolution when exported back to
-- Expr. Hidden dependency names need no visible primes or identity entries.
visibleGlobals :: Term -> Set Name
visibleGlobals (Variable Global expression) = Set.singleton (atomName expression)
visibleGlobals (Reference expression environment) =
  Set.fromList (atomName expression : map fst environment) <> foldMap (visibleGlobals . snd) environment
visibleGlobals (Syntax (Subst _ assignments) children) =
  Set.fromList (map fst assignments) <> foldMap visibleGlobals children
visibleGlobals term = foldMap visibleGlobals (termChildren term)

export :: (Fresh m) => Definitions -> Term -> m Expr
export _ (Variable _ expression) = pure expression
export _ (Reference expression []) = pure expression
export context (Reference expression environment) =
  Subst expression <$> mapM (\(key, value) -> (,) key <$> export context value) environment
export context (Syntax header children) = do
  (header', children') <- prepareScopes context (\_ -> foldMap visibleGlobals) header children
  withPathChildren header' <$> mapM (export context) children'

-- Each lexical scope is renamed once, including both quantifier children.
-- State tables are operations on Globals and never introduce Local bindings.
prepareScopes :: (Fresh m) => Definitions -> ([Name] -> [Term] -> Set Name) -> Expr -> [Term] -> m (Expr, [Term])
prepareScopes context incoming header children = foldM prepare (header, children) (scopeGroups header)
  where
    prepare (current, parts) (index, names, positions) = do
      let region = [parts !! position | position <- positions]
          collisions = filter (`Set.member` incoming names region) names
          avoid = forbidden context <> allNames current <> foldMap termNames parts <> Set.map nameToText (incoming names region)
      renaming <- allocateNames avoid collisions
      let renamed = [if position `elem` positions then renameLocals renaming part else part | (position, part) <- zip [0 ..] parts]
      pure (renameHeader index renaming current, renamed)

allocateNames :: (Fresh m) => Set Text -> [Name] -> m [(Text, Text)]
allocateNames _ [] = pure []
allocateNames avoid (name : rest) = do
  target <- freshAvoiding avoid (nameToText name)
  ((nameToText name, target) :) <$> allocateNames (Set.insert target avoid) rest

scopeGroups :: Expr -> [(Int, [Name], [Int])]
scopeGroups (Lam name _ _ _) = [(0, [name], [0])]
scopeGroups (Quant _ binders _ _ _) = [(1, map fst binders, [1, 2])]
scopeGroups (Case _ clauses _) = [(index, extractBinder pattern', [index]) | (index, CaseClause pattern' _) <- zip [1 ..] clauses]
scopeGroups _ = []

localBinders :: Expr -> Int -> [Name]
localBinders Subst {} _ = []
localBinders header index = childBinders header !! index

renameHeader :: Int -> [(Text, Text)] -> Expr -> Expr
renameHeader _ renaming (Lam name t body location) = Lam (renameName renaming name) t body location
renameHeader _ renaming (Quant op binders range body location) = Quant op [(renameName renaming name, t) | (name, t) <- binders] range body location
renameHeader index renaming expression@(Case scrutinee clauses location) =
  case clauses !? (index - 1) of
    Just (CaseClause pattern' body) ->
      case replaceAt (index - 1) (CaseClause (renamePatternBinders renaming pattern') body) clauses of
        Just clauses' -> Case scrutinee clauses' location
        Nothing -> expression
    Nothing -> expression
renameHeader _ _ expression = expression

renameLocals :: [(Text, Text)] -> Term -> Term
renameLocals = renameLocalsAs Local

renameLocalsAs :: Scope -> [(Text, Text)] -> Term -> Term
renameLocalsAs scope renaming (Variable Local (Var name t location))
  | nameToText name `elem` map fst renaming = Variable scope (Var (renameName renaming name) t location)
renameLocalsAs scope renaming (Variable Local (Const name t location))
  | nameToText name `elem` map fst renaming = Variable scope (Const (renameName renaming name) t location)
renameLocalsAs _ _ term@Variable {} = term
renameLocalsAs scope renaming (Reference expression environment) = Reference expression [(key, renameLocalsAs scope renaming value) | (key, value) <- environment]
renameLocalsAs scope renaming (Syntax header children) = Syntax header (zipWith child [0 ..] children)
  where
    child index = renameLocalsAs scope (filter (\(key, _) -> key `notElem` map nameToText (localBinders header index)) renaming)

termNames :: Term -> Set Text
termNames (Variable _ expression) = allNames expression
termNames (Reference expression environment) = allNames expression <> Set.fromList (map (nameToText . fst) environment) <> foldMap (termNames . snd) environment
termNames (Syntax header children) = allNames header <> foldMap termNames children

termChildren :: Term -> [Term]
termChildren Variable {} = []
termChildren (Reference _ environment) = map snd environment
termChildren (Syntax _ children) = children

withTermChildren :: Term -> [Term] -> Term
withTermChildren term@Variable {} [] = term
withTermChildren (Reference expression environment) children = Reference expression (zip (map fst environment) children)
withTermChildren (Syntax header _) children = Syntax header children
withTermChildren _ _ = error "Subst3: invalid child list"

atomName :: Expr -> Name
atomName (Var name _ _) = name
atomName (Const name _ _) = name
atomName _ = error "Subst3: a variable or reference must have a name"
