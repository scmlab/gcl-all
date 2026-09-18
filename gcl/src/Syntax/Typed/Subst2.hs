{-# LANGUAGE FlexibleContexts #-}

-- | Capture-avoiding substitution over the typed AST, in two passes.
--
--   'renameForSubstitution' first moves binders that would capture free names
--   brought in by a substitution. It changes binder names and their bound
--   occurrences, but does not insert replacement expressions. 'replace' then
--   performs simultaneous, scope-aware replacement without choosing names.
--   Renaming preserves each occurrence's type and source range; replacement
--   brings its own metadata.
--
--   What a binder scopes over, by node:
--
--   > \x -> body                   body
--   > <| op x : range : body |>    range and body, but not op
--   > case s of x -> body          each clause's body, but not s
--   > body [xs \ es]               body, but not es
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

-- | Substitution is simultaneous: a replacement is never rewritten by this
--   substitution. It is used whole, with its own type and range.
type Substitution = [(Text, Expr)]

type Renaming = [(Text, Text)]

-- | First alpha-rename the source tree, then insert the replacement expressions.
--   Duplicate substitution keys are rejected.
substExpr :: (Fresh m) => [(Text, Expr)] -> Expr -> m Expr
substExpr sb expr
  | hasDuplicateNames sb =
      error "substExpr: duplicate names in substitution"
  | otherwise =
      replace sb <$> renameForSubstitution sb expr

-- | Prepare an expression for the given substitution without applying it.
--   This is substitution-specific: which binders must move depends on the
--   free names of replacements that are visible in each binder's scope.
--   Only binders and their bound occurrences are renamed. The result is
--   alpha-equivalent to the input. 'replace' then needs no freshness check.
--   One caveat: an 'EHole' is copied unchanged, so renaming a surrounding
--   binder may leave its stored 'Env' out of sync with the transformed tree
--   (see below).
--   Duplicate substitution keys are rejected.
--
--   Examples (pseudo-GCL; primes stand for fresh names):
--
--   > (\x -> (x, y))[y := x]  ==>  \x' -> (x', y)
--   > (\x -> y)[y := z]       ==>  \x -> y
--   > (\x -> x)[x := y]       ==>  \x -> x
--   > ((\x -> x), y)[y := x]  ==>  ((\x -> x), y)
--
--   In the first example, @y@ is still present: only the capturing binder
--   and its bound @x@ are renamed. The other examples need no renaming:
--   the replacement cannot be captured, the entry is shadowed, or its
--   occurrence lies outside the binder's scope, respectively.
--
--   With multiple substitution entries, the same rule applies:
--
--   > (\x -> \y -> (a, b))[a := x, b := y]  ==>  \x' -> \y' -> (a, b)
--   > (\x -> (x, a, b))[a := x, b := z]     ==>  \x' -> (x', a, b)
--
--   Both @a@ and @b@ remain pending; only binders that could capture a
--   replacement move.
--
--   Binders that are not lambdas follow the same rule:
--
--   > (case c of x -> (x, y))[y := x]  ==>  case c of x' -> (x', y)
renameForSubstitution :: (Fresh m) => [(Text, Expr)] -> Expr -> m Expr
renameForSubstitution sb expr
  | hasDuplicateNames sb =
      error "renameForSubstitution: duplicate names in substitution"
  | otherwise = prepare sb expr

hasDuplicateNames :: Substitution -> Bool
hasDuplicateNames sb =
  Set.size (Set.fromList (map fst sb)) /= length sb

prepare :: (Fresh m) => Substitution -> Expr -> m Expr
prepare _ e@Lit {} = pure e
prepare _ e@Var {} = pure e
prepare _ e@Const {} = pure e
prepare _ e@Op {} = pure e
prepare sb (Chain chain) = Chain <$> prepareChain sb chain
prepare sb (App function argument l) =
  App <$> prepare sb function <*> prepare sb argument <*> pure l
prepare sb (Lam x t body l) = do
  (binderRenaming, sb') <- prepareBinders sb [x] [body]
  body' <- prepare sb' (renameFree binderRenaming body)
  pure (Lam (renameName binderRenaming x) t body' l)
prepare sb (Tuple elements) = Tuple <$> mapM (prepare sb) elements
prepare sb (OutT index e) = OutT index <$> prepare sb e
prepare sb (Quant operator binders range body l) = do
  operator' <- prepare sb operator
  (binderRenaming, sb') <- prepareBinders sb (map fst binders) [range, body]
  range' <- prepare sb' (renameFree binderRenaming range)
  body' <- prepare sb' (renameFree binderRenaming body)
  pure
    ( Quant
        operator'
        [(renameName binderRenaming x, t) | (x, t) <- binders]
        range'
        body'
        l
    )
prepare sb (ArrIdx array index l) =
  ArrIdx <$> prepare sb array <*> prepare sb index <*> pure l
prepare sb (ArrUpd array index value l) =
  ArrUpd
    <$> prepare sb array
    <*> prepare sb index
    <*> prepare sb value
    <*> pure l
prepare sb (Case scrutinee clauses l) =
  Case
    <$> prepare sb scrutinee
    <*> mapM (prepareClause sb) clauses
    <*> pure l
prepare sb (Subst body table) = do
  (binderRenaming, sb') <- prepareBinders sb (map fst table) [body]
  body' <- prepare sb' (renameFree binderRenaming body)
  values' <- mapM (\(x, e) -> (,) (renameName binderRenaming x) <$> prepare sb e) table
  pure (Subst body' values')
-- A hole is opaque. Its 'Env' is a snapshot of the scope in the elaborated
-- source tree, so this traversal does not rewrite it when surrounding binders
-- are renamed. Consequently, the 'Env' in a transformed copy may not match
-- the surrounding AST and must not be used for scope-sensitive operations
-- such as hole refinement. The server retains source-derived holes separately
-- for that purpose; see 'GCL.WP.sweep'.
prepare _ e@EHole {} = pure e

prepareChain :: (Fresh m) => Substitution -> Chain -> m Chain
prepareChain sb (Pure e) = Pure <$> prepare sb e
prepareChain sb (More chain operator t e) =
  More <$> prepareChain sb chain <*> pure operator <*> pure t <*> prepare sb e

-- | A clause's pattern binds over its body only, never over the scrutinee.
prepareClause :: (Fresh m) => Substitution -> CaseClause -> m CaseClause
prepareClause sb (CaseClause pattern' body) = do
  (binderRenaming, sb') <- prepareBinders sb (extractBinder pattern') [body]
  body' <- prepare sb' (renameFree binderRenaming body)
  pure (CaseClause (renamePatternBinders binderRenaming pattern') body')

-- | Apply the substitution after all capture risks have been removed.
--   Binders still hide entries with the same name; replacement
--   expressions are inserted whole, without recursively substituting into them.
replace :: Substitution -> Expr -> Expr
replace _ e@Lit {} = e
replace sb (Var x t l) = occurrence sb Var x t l
replace sb (Const x t l) = occurrence sb Const x t l
replace _ e@Op {} = e
replace sb (Chain chain) = Chain (replaceChain sb chain)
replace sb (App function argument l) =
  App (replace sb function) (replace sb argument) l
replace sb (Lam x t body l) =
  Lam x t (replace (hide [x] sb) body) l
replace sb (Tuple elements) = Tuple (map (replace sb) elements)
replace sb (OutT index e) = OutT index (replace sb e)
-- The operator lies outside the binders' scope.
replace sb (Quant operator binders range body l) =
  Quant
    (replace sb operator)
    binders
    (replace inner range)
    (replace inner body)
    l
  where
    inner = hide (map fst binders) sb
replace sb (ArrIdx array index l) =
  ArrIdx (replace sb array) (replace sb index) l
replace sb (ArrUpd array index value l) =
  ArrUpd (replace sb array) (replace sb index) (replace sb value) l
replace sb (Case scrutinee clauses l) =
  Case (replace sb scrutinee) (map (replaceClause sb) clauses) l
-- The table's domain binds over the body only, never over its values.
replace sb (Subst body table) =
  Subst
    (replace (hide (map fst table) sb) body)
    [(x, replace sb e) | (x, e) <- table]
replace _ e@EHole {} = e

replaceChain :: Substitution -> Chain -> Chain
replaceChain sb (Pure e) = Pure (replace sb e)
replaceChain sb (More chain operator t e) =
  More (replaceChain sb chain) operator t (replace sb e)

replaceClause :: Substitution -> CaseClause -> CaseClause
replaceClause sb (CaseClause pattern' body) =
  CaseClause pattern' (replace (hide (extractBinder pattern') sb) body)

occurrence ::
  Substitution ->
  (Name -> Type -> Maybe Range -> Expr) ->
  Name ->
  Type ->
  Maybe Range ->
  Expr
occurrence sb build name t l =
  case lookup (nameToText name) sb of
    Nothing -> build name t l
    Just e -> e

-- | A binder shadows entries for its names throughout its scope.
hide :: [Name] -> [(Text, a)] -> [(Text, a)]
hide binders = filter (\(key, _) -> Set.notMember key bound)
  where
    bound = Set.fromList (map nameToText binders)

-- | Decide which binders would capture a visible replacement. The chosen
--   names are absent from the region, including its inner binders, because
--   'renameFree' itself never allocates fresh names. Binder names must be
--   distinct; type inference enforces this for source ASTs.
prepareBinders :: (Fresh m) => Substitution -> [Name] -> [Expr] -> m (Renaming, Substitution)
prepareBinders sb binders region = do
  binderRenaming <- allocate forbidden clashing
  pure (binderRenaming, visible)
  where
    bound = Set.fromList (map nameToText binders)
    freeInRegion = foldMap freeVarsT region

    -- A binder hides its own entry. An entry whose name is not free in this
    -- region cannot insert anything here.
    visible =
      filter
        (\(key, _) -> Set.notMember key bound && Set.member key freeInRegion)
        sb

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
