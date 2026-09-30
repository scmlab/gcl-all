{-# LANGUAGE FlexibleContexts #-}

-- | Capture-avoiding substitution over the typed AST.
--
-- == Textbook rules
--
-- Peter Selinger's /Lecture Notes on the Lambda Calculus/ defines @M[N\/x]@,
-- the substitution of @N@ for the free occurrences of @x@ in @M@, by:
--
-- > S1.  x[N/x]        =  N
-- > S2.  y[N/x]        =  y                         if x /= y
-- > S3.  (M P)[N/x]    =  (M[N/x]) (P[N/x])
-- > S4.  (\x.M)[N/x]   =  \x.M
-- > S5.  (\y.M)[N/x]   =  \y.(M[N/x])               if x /= y and y not in FV(N)
-- > S6.  (\y.M)[N/x]   =  \y'.(M{y'/y}[N/x])        if x /= y, y in FV(N), and y' fresh
--
-- The renaming @M{y\/x}@ used by S6 is defined by:
--
-- > R1.  x{y/x}        =  y
-- > R2.  z{y/x}        =  z                         if x /= z
-- > R3.  (M N){y/x}    =  (M{y/x}) (N{y/x})
-- > R4.  (\x.M){y/x}   =  \y.(M{y/x})
-- > R5.  (\z.M){y/x}   =  \z.(M{y/x})               if x /= z
--
-- S6 is the only substitution rule that invokes this renaming. Its fresh
-- target @y'@ must not occur anywhere in @M@; under that precondition,
-- R1-R5 can replace binding, bound, and free occurrences uniformly.
-- These compact rules are well suited to proofs and to defining
-- alpha-equivalence. This module adjusts them because its result is also shown
-- to users, for whom a needless prime is needless cognitive overhead.
--
-- The textbook writes the replacement first, @[N\/x]@. The public examples in
-- this module use the implementation's key-first notation, @[x := N]@.
--
-- == Minimal renaming
--
-- The intended rule is an if-and-only-if:
--
-- > binder b is renamed
-- > iff some substitution [x := N] really fires in b's scope
-- >     and b is in FV(N).
--
-- For a single binder @y@, this means @x /= y@, @x@ is free in the body @M@,
-- and @y@ is free in @N@. Thus every rename prevents a concrete capture, and
-- every concrete capture is prevented.
--
-- === Modified S5 and S6
--
-- The textbook's S6 can rename a binder even when there is no @x@ to replace:
--
-- > (\y -> no_x)[x := y + 1]
-- > textbook:  \y' -> no_x
-- > here:      \y  -> no_x
--
-- No replacement is inserted, so no @y@ can be captured and the binder does
-- not need to be renamed. We therefore add the test @x in FV(M)@ and use these
-- rules:
--
-- > S5'. (\y.M)[N/x]   =  \y.(M[N/x])               if x /= y
-- >                                                    and (y not in FV(N) or x not in FV(M))
-- > S6'. (\y.M)[N/x]   =  \y'.(M{y'/y}[N/x])        if x /= y, y in FV(N),
-- >                                                    x in FV(M), and y' fresh
--
-- Among the @x /= y@ cases, S6' therefore does less than S6: cases where @x@
-- is not free in @M@ move to S5'. S4 is unchanged.
--
-- === Modified R4
--
-- In @M{y'/y}@, textbook renaming replaces all occurrences of @y@, whether
-- free, bound, or binding. R4 therefore also renames an unrelated inner binder:
--
-- > (\y -> (\y -> y) x)[x := y]
-- > textbook:  \y' -> ((\y' -> y') y)
-- > here:      \y' -> ((\y  -> y)  y)
--
-- The inner @\y -> y@ belongs to its own binder, not the outer binder being
-- alpha-renamed. When applying @x -> y@, 'renameFree' therefore leaves a binder
-- named @x@ and its scope unchanged:
--
-- > R4.   (\x.M){y/x}  =  \y.(M{y/x})    -- textbook
-- > R4'.  (\x.M){y/x}  =  \x.M           -- this module
--
-- The caller separately renames the binder it selected. Together, changing
-- that binder and applying 'renameFree' to its scope form the alpha-renaming.
--
-- == Extension to GCL
--
-- GCL extends the lambda-calculus rules in four ways:
--
-- * It has several binding forms: 'Lam', 'CaseClause', 'Quant', and 'Subst'.
--
-- * A simultaneous substitution can contain several entries,
--   e.g. @[x := M, y := N]@.
--
-- * A node can introduce several binders, e.g.
--   @case (a, b) of (binder1, binder2) -> body@.
--
-- * A binder's scope can cover several subexpressions, e.g. both @range@ and
--   @body@ in @<| + i : range : body |>@.
--
-- What a binder scopes over, by node:
--
-- > \x -> body                   body
-- > <| op x : range : body |>    range and body, but not op
-- > case s of x -> body          each clause's body, but not s
-- > body [xs \ es]               body, but not es
--
-- 'underBinders' applies S4, S5', and S6' uniformly to those forms. It keeps
-- only entries that can fire in the scoped region, gathers the free names their
-- replacements can introduce, and freshens exactly the binders that clash.
-- Fresh targets cannot collide with an active substitution key; this invariant
-- is needed because 'renameFree' runs before 'subst' continues into the region.
--
-- Renaming preserves each occurrence's type and source range; a replacement
-- brings its own metadata.
module Syntax.Typed.Subst2 (substExpr, renameFree) where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (Fresh (..), freeVarsT)
import Syntax.Abstract.Types (Pattern (..), extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Instances.Free ()
import Syntax.Typed.Types

-- | Substitution is simultaneous: a replacement is never rewritten by this
--   substitution. It is used whole, with its own type and range.
type Substitution = [(Text, Expr)]

-- | Renaming only changes how a name is spelled, so an occurrence keeps the
--   type and range it already had. The names being rewritten must be distinct;
--   internal callers satisfy this because binder names are distinct.
type Renaming = [(Text, Text)]

-- | Substitute simultaneously, avoiding capture. Duplicate substitution keys
--   are rejected.
--
--   The expression is treated as a scope root. For a detached subtree, callers
--   must account for enclosing binders that may shadow keys or capture
--   replacements.
--
--   One caveat: an 'EHole' is copied unchanged, so renaming a surrounding
--   binder may leave its stored 'Env' out of sync with the transformed tree.
--   Such a stale 'Env' must not be used for scope-sensitive work such as hole
--   refinement; the 'EHole' case of 'subst' says where the server gets holes
--   it can trust.
--
--   Examples (pseudo-GCL, in this module's @[key := value]@ notation; primes
--   stand for fresh names):
--
--   > (\x -> (x, y))[y := x]  ==>  \x' -> (x', x)
--   > (\x -> y)[y := z]       ==>  \x -> z
--   > (\x -> x)[x := y]       ==>  \x -> x
--   > ((\x -> x), y)[y := x]  ==>  ((\x -> x), x)
--
--   Only the first needs a binder to be renamed: in the others the replacement
--   cannot be captured, the entry is shadowed, or the occurrence lies outside
--   the binder's scope, respectively.
substExpr :: (Fresh m) => [(Text, Expr)] -> Expr -> m Expr
substExpr sb expr
  | hasDuplicateNames sb =
      error "substExpr: duplicate names in substitution"
  | otherwise = subst sb expr

hasDuplicateNames :: Substitution -> Bool
hasDuplicateNames sb =
  Set.size (Set.fromList (map fst sb)) /= length sb

-- | Recursive worker for 'substExpr'. Entries in @sb@ replace free occurrences
--   simultaneously. At a binder, the existing region is alpha-renamed before
--   replacements are inserted, so newly inserted free names are not renamed.
subst :: (Fresh m) => Substitution -> Expr -> m Expr
subst _ e@Lit {} = pure e
subst sb e@(Var x _ _) = pure (fromMaybe e (lookup (nameToText x) sb))
subst sb e@(Const x _ _) = pure (fromMaybe e (lookup (nameToText x) sb))
subst _ e@Op {} = pure e
subst sb (Chain chain) = Chain <$> substChain sb chain
subst sb (App function argument l) =
  App <$> subst sb function <*> subst sb argument <*> pure l
subst sb (Lam x t body l) = do
  (renaming, sb') <- underBinders sb [x] [body]
  body' <- subst sb' (renameFree renaming body)
  pure (Lam (renameName renaming x) t body' l)
subst sb (Tuple elements) = Tuple <$> mapM (subst sb) elements
subst sb (OutT index e) = OutT index <$> subst sb e
subst sb (Quant operator binders range body l) = do
  operator' <- subst sb operator -- outside the binders' scope
  (renaming, sb') <- underBinders sb (map fst binders) [range, body]
  range' <- subst sb' (renameFree renaming range)
  body' <- subst sb' (renameFree renaming body)
  pure
    ( Quant
        operator'
        [(renameName renaming x, t) | (x, t) <- binders]
        range'
        body'
        l
    )
subst sb (ArrIdx array index l) =
  ArrIdx <$> subst sb array <*> subst sb index <*> pure l
subst sb (ArrUpd array index value l) =
  ArrUpd
    <$> subst sb array
    <*> subst sb index
    <*> subst sb value
    <*> pure l
subst sb (Case scrutinee clauses l) =
  Case
    <$> subst sb scrutinee
    <*> mapM (substClause sb) clauses
    <*> pure l
-- The table's domain binds over the body only, never over its values.
subst sb (Subst body table) = do
  (renaming, sb') <- underBinders sb (map fst table) [body]
  body' <- subst sb' (renameFree renaming body)
  table' <- mapM (\(x, e) -> (,) (renameName renaming x) <$> subst sb e) table
  pure (Subst body' table')
-- A hole is opaque. Its 'Env' is a snapshot of the scope in the elaborated
-- source tree, so this traversal does not rewrite it when surrounding binders
-- are renamed; 'substExpr' states what that costs the caller. The server
-- retains source-derived holes separately for refinement; see 'GCL.WP.sweep'.
subst _ e@EHole {} = pure e

substChain :: (Fresh m) => Substitution -> Chain -> m Chain
substChain sb (Pure e) = Pure <$> subst sb e
substChain sb (More chain operator t e) =
  More <$> substChain sb chain <*> pure operator <*> pure t <*> subst sb e

-- | A clause's pattern binds over its body only, never over the scrutinee.
substClause :: (Fresh m) => Substitution -> CaseClause -> m CaseClause
substClause sb (CaseClause pattern' body) = do
  (renaming, sb') <- underBinders sb (extractBinder pattern') [body]
  body' <- subst sb' (renameFree renaming body)
  pure (CaseClause (renamePatternBinders renaming pattern') body')

-- | Choose between the three binder rules for a node that binds @binders@ over
--   @region@. Returns the renaming to apply to the region and the substitution
--   that is still active inside it. An empty renaming means no binder had to
--   be renamed.
--
--   The binders are the ones this node introduces -- a quantifier, a pattern
--   or a substitution table can bind several at once -- not those gathered on
--   the way down. Enclosing binders need no mention here: entries shadowed by
--   them have already been dropped from the substitution, and any renaming
--   they required has already been applied to the region. An enclosing name
--   can only be captured here if it occurs in the region, where 'allNames'
--   already forbids it.
--
--   Binder names must be distinct; type inference enforces this for source
--   ASTs.
underBinders :: (Fresh m) => Substitution -> [Name] -> [Expr] -> m (Renaming, Substitution)
underBinders sb binders region = do
  binderRenaming <- allocate forbidden clashingBinders
  pure (binderRenaming, activeSb)
  where
    bound = Set.fromList (map nameToText binders)
    freeInRegion = foldMap freeVarsT region

    -- Entries that can still fire in this region. The first condition is S4:
    -- a binder shadows an entry with the same key. The second is the added
    -- @x in FV(M)@ condition in S5'/S6'.
    activeSb =
      filter
        (\(key, _) -> Set.notMember key bound && Set.member key freeInRegion)
        sb

    incoming = foldMap (freeVarsT . snd) activeSb
    clashingBinders = filter (\binder -> Set.member (nameToText binder) incoming) binders

    -- 'renameFree' requires targets that occur nowhere in the region, binders
    -- included, which is why 'allNames' and not just the free names.
    --
    -- The free-in-region condition above also establishes an invariant needed
    -- by this single traversal: every entry carried into the region has its
    -- key in @allNames region@. A fresh target therefore cannot equal a carried
    -- key. Without that invariant, 'renameFree' could create occurrences that
    -- the following 'subst' mistakes for substitution occurrences.
    forbidden = incoming <> foldMap allNames region <> bound

-- | A target for each binder. Each target joins @forbidden@ before the next
--   one is chosen, so binders of the same node cannot land on a common name.
allocate :: (Fresh m) => Set Text -> [Name] -> m Renaming
allocate _ [] = pure []
allocate forbidden (binder : rest) = do
  target <- freshFor forbidden binder
  ((nameToText binder, target) :) <$> allocate (Set.insert target forbidden) rest

-- | A name outside @forbidden@. 'Fresh' only proposes one: 'Fresh WP' avoids
--   the names in its own reader scopes and hands back the prefix unchanged for
--   everything else, so every proposal is checked here.
--
--   A clash extends the prefix rather than asking again, because that instance
--   is reader-only and would answer identically forever. This loop terminates
--   if every 'freshPre' result is at least as long as its prefix: @forbidden@
--   is finite, so a long enough prefix outgrows every member. All three
--   current instances satisfy this; the 'Fresh' class does not require it.
freshFor :: (Fresh m) => Set Text -> Name -> m Text
freshFor forbidden binder = go (nameToText binder)
  where
    go prefix = do
      candidate <- freshPre prefix
      if Set.member candidate forbidden
        then go (Text.snoc prefix '\'')
        else pure candidate

-- | Implement R4': for each renaming @x -> y@, rewrite free occurrences of @x@
--   as @y@. A binder named @x@ disables that renaming within its scope. The
--   caller rewrites the binder declaration separately.
--
--   'renameFree' does not choose fresh targets; it assumes they are already
--   fresh for the expression. In particular, a target must not occur as an
--   inner binder, or that binder would capture the newly renamed occurrences.
--   'underBinders' chooses targets that satisfy this precondition.
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

-- | A binder shadows entries for its names throughout its scope.
hide :: [Name] -> [(Text, a)] -> [(Text, a)]
hide binders = filter (\(key, _) -> Set.notMember key bound)
  where
    bound = Set.fromList (map nameToText binders)

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

-- | Rewrite the binders a pattern introduces. 'renameFree' leaves every binder
--   alone, so the caller renames them here.
renamePatternBinders :: Renaming -> Pattern -> Pattern
renamePatternBinders _ pattern'@PattLit {} = pattern'
renamePatternBinders renaming (PattBinder x) = PattBinder (renameName renaming x)
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
