{-# LANGUAGE FlexibleContexts #-}

-- | Capture-avoiding substitution over the typed AST, in two passes.
--
--   'renameForSubstitution' first moves binders that would capture free names
--   brought in by a substitution. It changes binder names and their bound
--   occurrences, but does not insert replacement expressions. 'replace' then
--   performs simultaneous, scope-aware replacement without choosing names.
--   Renaming preserves each occurrence's type and source range; replacement
--   brings its own metadata.
module Syntax.Typed.Subst2 (substExpr, renameForSubstitution) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (Fresh (..), freeVarsT)
import GCL.Range (Range)
import Syntax.Abstract.Types (Pattern (..), Type, extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Instances.Free ()
import Syntax.Typed.Types

-- | Assignments are simultaneous: their values are never rewritten by this
--   substitution. A replacement is used whole, with its type and range.
type Substitution = [(Text, Expr)]

type Renaming = [(Text, Text)]

-- | First alpha-rename the source tree, then insert the replacement expressions.
substExpr :: (Fresh m) => [(Text, Expr)] -> Expr -> m Expr
substExpr assignments expr
  | hasDuplicateAssignments assignments =
      error "substExpr: duplicate assignment names"
  | otherwise =
      replace assignments <$> renameForSubstitution assignments expr

-- | Prepare an expression for the given substitution without applying it.
--   This is substitution-specific: which binders must move depends on the
--   free names of replacements that are visible in each binder's scope.
--   Duplicate assignment names are rejected.
renameForSubstitution :: (Fresh m) => Substitution -> Expr -> m Expr
renameForSubstitution assignments expr
  | hasDuplicateAssignments assignments =
      error "renameForSubstitution: duplicate assignment names"
  | otherwise = prepare assignments expr

hasDuplicateAssignments :: Substitution -> Bool
hasDuplicateAssignments assignments =
  Set.size (Set.fromList (map fst assignments)) /= length assignments

prepare :: (Fresh m) => Substitution -> Expr -> m Expr
prepare _ e@Lit {} = pure e
prepare _ e@Var {} = pure e
prepare _ e@Const {} = pure e
prepare _ e@Op {} = pure e
prepare sub (Chain chain) = Chain <$> prepareChain sub chain
prepare sub (App function argument l) =
  App <$> prepare sub function <*> prepare sub argument <*> pure l
prepare sub (Lam x t body l) = do
  (binderRenaming, innerSub) <- prepareBinders sub [x] [body]
  body' <- prepare innerSub (renameFree binderRenaming body)
  pure (Lam (renameName binderRenaming x) t body' l)
prepare sub (Tuple elements) = Tuple <$> mapM (prepare sub) elements
prepare sub (OutT index e) = OutT index <$> prepare sub e
prepare sub (Quant operator binders range body l) = do
  operator' <- prepare sub operator
  (binderRenaming, innerSub) <- prepareBinders sub (map fst binders) [range, body]
  range' <- prepare innerSub (renameFree binderRenaming range)
  body' <- prepare innerSub (renameFree binderRenaming body)
  pure
    ( Quant
        operator'
        [(renameName binderRenaming x, t) | (x, t) <- binders]
        range'
        body'
        l
    )
prepare sub (ArrIdx array index l) =
  ArrIdx <$> prepare sub array <*> prepare sub index <*> pure l
prepare sub (ArrUpd array index value l) =
  ArrUpd
    <$> prepare sub array
    <*> prepare sub index
    <*> prepare sub value
    <*> pure l
prepare sub (Case scrutinee clauses l) =
  Case
    <$> prepare sub scrutinee
    <*> mapM (prepareClause sub) clauses
    <*> pure l
prepare sub (Subst body table) = do
  (binderRenaming, innerSub) <- prepareBinders sub (map fst table) [body]
  body' <- prepare innerSub (renameFree binderRenaming body)
  values' <- mapM (\(x, e) -> (,) (renameName binderRenaming x) <$> prepare sub e) table
  pure (Subst body' values')
-- A hole is opaque. Its 'Env' is a snapshot of the scope in the elaborated
-- source tree, so this traversal does not rewrite it when surrounding binders
-- are renamed. Consequently, the 'Env' in a transformed copy may not match
-- the surrounding AST and must not be used for scope-sensitive operations
-- such as hole refinement. The server retains source-derived holes separately
-- for that purpose; see 'GCL.WP.sweep'.
prepare _ e@EHole {} = pure e

prepareChain :: (Fresh m) => Substitution -> Chain -> m Chain
prepareChain sub (Pure e) = Pure <$> prepare sub e
prepareChain sub (More chain operator t e) =
  More <$> prepareChain sub chain <*> pure operator <*> pure t <*> prepare sub e

-- | A clause's pattern binds over its body only, never over the scrutinee.
prepareClause :: (Fresh m) => Substitution -> CaseClause -> m CaseClause
prepareClause sub (CaseClause pattern' body) = do
  (binderRenaming, innerSub) <- prepareBinders sub (extractBinder pattern') [body]
  body' <- prepare innerSub (renameFree binderRenaming body)
  pure (CaseClause (renamePatternBinders binderRenaming pattern') body')

-- | Apply the substitution after all capture risks have been removed.
--   Bindings still hide assignments with the same domain name; replacement
--   expressions are inserted whole, without recursively substituting into them.
replace :: Substitution -> Expr -> Expr
replace _ e@Lit {} = e
replace sub (Var x t l) = occurrence sub Var x t l
replace sub (Const x t l) = occurrence sub Const x t l
replace _ e@Op {} = e
replace sub (Chain chain) = Chain (replaceChain sub chain)
replace sub (App function argument l) =
  App (replace sub function) (replace sub argument) l
replace sub (Lam x t body l) =
  Lam x t (replace (hide [x] sub) body) l
replace sub (Tuple elements) = Tuple (map (replace sub) elements)
replace sub (OutT index e) = OutT index (replace sub e)
-- The operator lies outside the binders' scope.
replace sub (Quant operator binders range body l) =
  Quant
    (replace sub operator)
    binders
    (replace inner range)
    (replace inner body)
    l
  where
    inner = hide (map fst binders) sub
replace sub (ArrIdx array index l) =
  ArrIdx (replace sub array) (replace sub index) l
replace sub (ArrUpd array index value l) =
  ArrUpd (replace sub array) (replace sub index) (replace sub value) l
replace sub (Case scrutinee clauses l) =
  Case (replace sub scrutinee) (map (replaceClause sub) clauses) l
-- The table's domain binds over the body only, never over its values.
replace sub (Subst body table) =
  Subst
    (replace (hide (map fst table) sub) body)
    [(x, replace sub e) | (x, e) <- table]
replace _ e@EHole {} = e

replaceChain :: Substitution -> Chain -> Chain
replaceChain sub (Pure e) = Pure (replace sub e)
replaceChain sub (More chain operator t e) =
  More (replaceChain sub chain) operator t (replace sub e)

replaceClause :: Substitution -> CaseClause -> CaseClause
replaceClause sub (CaseClause pattern' body) =
  CaseClause pattern' (replace (hide (extractBinder pattern') sub) body)

occurrence ::
  Substitution ->
  (Name -> Type -> Maybe Range -> Expr) ->
  Name ->
  Type ->
  Maybe Range ->
  Expr
occurrence sub build name t l =
  case lookup (nameToText name) sub of
    Nothing -> build name t l
    Just e -> e

-- | A binder shadows assignments for its names throughout its scope.
hide :: [Name] -> [(Text, a)] -> [(Text, a)]
hide binders = filter (\(key, _) -> Set.notMember key bound)
  where
    bound = Set.fromList (map nameToText binders)

-- | Decide which binders would capture a visible replacement. The chosen
--   names are absent from the region, including its inner binders, because
--   'renameFree' itself never allocates fresh names. Binder names must be
--   distinct; type inference enforces this for source ASTs.
prepareBinders :: (Fresh m) => Substitution -> [Name] -> [Expr] -> m (Renaming, Substitution)
prepareBinders sub binders region = do
  binderRenaming <- allocate forbidden clashing
  pure (binderRenaming, visible)
  where
    bound = Set.fromList (map nameToText binders)
    freeInRegion = foldMap freeVarsT region

    -- A binder hides its own assignment. An assignment whose domain is not
    -- free in this region cannot insert anything here.
    visible =
      filter
        (\(key, _) -> Set.notMember key bound && Set.member key freeInRegion)
        sub

    incoming = foldMap (freeVarsT . snd) visible
    clashing = filter (\binder -> Set.member (nameToText binder) incoming) binders
    forbidden = incoming <> foldMap allNames region <> bound

allocate :: (Fresh m) => Set Text -> [Name] -> m Renaming
allocate _ [] = pure []
allocate forbidden (binder : rest) = do
  target <- freshFor forbidden binder
  ((nameToText binder, target) :) <$> allocate (Set.insert target forbidden) rest

-- | 'Fresh WP' may propose a name already present in the expression. Check
--   every proposal explicitly; a rejected prefix is extended before retrying.
freshFor :: (Fresh m) => Set Text -> Name -> m Text
freshFor forbidden binder = go (nameToText binder)
  where
    go prefix = do
      candidate <- freshPre prefix
      if Set.member candidate forbidden
        then go (Text.snoc prefix '\'')
        else pure candidate

-- | Change free occurrences relative to the supplied region. The caller has
--   already chosen targets absent from that region. Binders are updated
--   separately; nested binders shadow the renaming in their own scopes.
renameFree :: Renaming -> Expr -> Expr
renameFree _ e@Lit {} = e
renameFree renaming (Var x t l) = Var (renameName renaming x) t l
renameFree renaming (Const x t l) = Const (renameName renaming x) t l
renameFree _ e@Op {} = e
renameFree renaming (Chain chain) = Chain (renameChain renaming chain)
renameFree renaming (App function argument l) =
  App (renameFree renaming function) (renameFree renaming argument) l
renameFree renaming (Lam x t body l) =
  Lam x t (renameFree (hide [x] renaming) body) l
renameFree renaming (Tuple elements) = Tuple (map (renameFree renaming) elements)
renameFree renaming (OutT index e) = OutT index (renameFree renaming e)
-- The operator lies outside the binders' scope.
renameFree renaming (Quant operator binders range body l) =
  Quant
    (renameFree renaming operator)
    binders
    (renameFree inner range)
    (renameFree inner body)
    l
  where
    inner = hide (map fst binders) renaming
renameFree renaming (ArrIdx array index l) =
  ArrIdx (renameFree renaming array) (renameFree renaming index) l
renameFree renaming (ArrUpd array index value l) =
  ArrUpd
    (renameFree renaming array)
    (renameFree renaming index)
    (renameFree renaming value)
    l
renameFree renaming (Case scrutinee clauses l) =
  Case (renameFree renaming scrutinee) (map (renameClause renaming) clauses) l
-- The table's domain binds over the body only, never over its values.
renameFree renaming (Subst body table) =
  Subst
    (renameFree (hide (map fst table) renaming) body)
    [(x, renameFree renaming e) | (x, e) <- table]
renameFree _ e@EHole {} = e

renameChain :: Renaming -> Chain -> Chain
renameChain renaming (Pure e) = Pure (renameFree renaming e)
renameChain renaming (More chain operator t e) =
  More (renameChain renaming chain) operator t (renameFree renaming e)

renameClause :: Renaming -> CaseClause -> CaseClause
renameClause renaming (CaseClause pattern' body) =
  CaseClause pattern' (renameFree (hide (extractBinder pattern') renaming) body)

renameName :: Renaming -> Name -> Name
renameName renaming name@(Name text range) =
  case lookup text renaming of
    Nothing -> name
    Just text' -> Name text' range

renamePatternBinders :: Renaming -> Pattern -> Pattern
renamePatternBinders _ pattern'@PattLit {} = pattern'
renamePatternBinders renaming (PattBinder x) =
  PattBinder (renameName renaming x)
renamePatternBinders _ pattern'@PattWildcard {} = pattern'
renamePatternBinders renaming (PattTuple patterns) =
  PattTuple (map (renamePatternBinders renaming) patterns)
-- A constructor name is not a binder.
renamePatternBinders renaming (PattConstructor constructor patterns) =
  PattConstructor constructor (map (renamePatternBinders renaming) patterns)

-- | Term names and binder names in a region. Free names alone are insufficient
--   when a proposed target could collide with an inner binder.
allNames :: Expr -> Set Text
allNames Lit {} = mempty
allNames (Var x _ _) = Set.singleton (nameToText x)
allNames (Const x _ _) = Set.singleton (nameToText x)
allNames Op {} = mempty
allNames (Chain chain) = allNamesChain chain
allNames (App function argument _) = allNames function <> allNames argument
allNames (Lam x _ body _) = Set.insert (nameToText x) (allNames body)
allNames (Tuple elements) = foldMap allNames elements
allNames (OutT _ e) = allNames e
allNames (Quant operator binders range body _) =
  allNames operator
    <> Set.fromList (map (nameToText . fst) binders)
    <> allNames range
    <> allNames body
allNames (ArrIdx array index _) = allNames array <> allNames index
allNames (ArrUpd array index value _) =
  allNames array <> allNames index <> allNames value
allNames (Case scrutinee clauses _) =
  allNames scrutinee <> foldMap allNamesClause clauses
allNames (Subst body table) =
  allNames body
    <> Set.fromList (map (nameToText . fst) table)
    <> foldMap (allNames . snd) table
allNames EHole {} = mempty

allNamesChain :: Chain -> Set Text
allNamesChain (Pure e) = allNames e
allNamesChain (More chain _ _ e) = allNamesChain chain <> allNames e

allNamesClause :: CaseClause -> Set Text
allNamesClause (CaseClause pattern' body) =
  Set.fromList (map nameToText (extractBinder pattern')) <> allNames body
