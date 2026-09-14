{-# LANGUAGE FlexibleContexts #-}

-- | Capture-avoiding substitution over the typed AST, in one traversal.
--
--   Substituting and alpha-renaming are different operations that a single
--   @[(Text, Expr)]@ is forced to conflate. A substitution puts an arbitrary
--   expression in place of a name, so its range has to be a full 'Expr'. A
--   renaming only puts another name there, and must leave the occurrence's own
--   type and source range alone.
--
--   The usual trick -- encoding a renaming as a substitution by @Var x'@, as
--   'Syntax.Substitution' does through @mkVar@ -- has to build that 'Expr' at
--   the binder, and this AST does not have what that takes. A 'PattBinder'
--   carries no 'Type', because the typed AST reuses @Syntax.Abstract@'s
--   untyped 'Pattern', so no replacement expression can be built for one at
--   all. And an occurrence's 'Name' carries the range that
--   @Render.Syntax.Common@ turns into a link back into the source, so a
--   replacement built once at the binder would collapse every occurrence of a
--   renamed variable onto the binder's position.
--
--   So the two operations stay apart while sharing one environment: each
--   entry contains one 'SubstAction'. Encoding that choice in the type keeps
--   it from becoming an invariant that callers must maintain by hand.
module Syntax.Typed.Subst2 (substExpr) where

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

-- | What to do when a name is encountered during substitution.
data SubstAction
  = -- | Rename @x@ to this name. Only the name changes, so the
    --   occurrence keeps the type and range it already had.
    RenameTo Text
  | -- | Replace @x@ with this expression.
    ReplaceWith Expr
  deriving (Show)

-- | The actions a traversal carries into a binder's scope.
type SubstEnv = [(Text, SubstAction)]

-- | The free names an action would carry into a scope. 'RenameTo' carries its
--   target name; 'ReplaceWith' carries the replacement's free names.
carriedIn :: SubstAction -> Set Text
carriedIn (RenameTo x) = Set.singleton x
carriedIn (ReplaceWith e) = freeVarsT e

-- | Substitute, as callers of the @Substitutable@ instance mean it.
--   Duplicate assignment names are rejected.
substExpr :: (Fresh m) => [(Text, Expr)] -> Expr -> m Expr
substExpr assignments
  | Set.size domain /= length assignments =
      error "substExpr: duplicate assignment names"
  | otherwise =
      substitute [(x, ReplaceWith e) | (x, e) <- assignments]
  where
    domain = Set.fromList (map fst assignments)

substitute :: (Fresh m) => SubstEnv -> Expr -> m Expr
substitute _ e@Lit {} = pure e
substitute env (Var x t l) = pure (occurrence env Var x t l)
substitute env (Const x t l) = pure (occurrence env Const x t l)
substitute _ e@Op {} = pure e
substitute env (Chain chain) = Chain <$> substituteChain env chain
substitute env (App function argument l) =
  App <$> substitute env function <*> substitute env argument <*> pure l
substitute env (Lam x t body l) = do
  (binderRenaming, innerEnv) <- underBinders env [x] (freeVarsT body)
  Lam (renameName binderRenaming x) t <$> substitute innerEnv body <*> pure l
substitute env (Tuple elements) = Tuple <$> mapM (substitute env) elements
substitute env (OutT index e) = OutT index <$> substitute env e
substitute env (Quant operator binders range body l) = do
  operator' <- substitute env operator
  (binderRenaming, innerEnv) <-
    underBinders env (map fst binders) (freeVarsT (range, body))
  Quant operator' [(renameName binderRenaming x, t) | (x, t) <- binders]
    <$> substitute innerEnv range
    <*> substitute innerEnv body
    <*> pure l
substitute env (ArrIdx array index l) =
  ArrIdx <$> substitute env array <*> substitute env index <*> pure l
substitute env (ArrUpd array index value l) =
  ArrUpd
    <$> substitute env array
    <*> substitute env index
    <*> substitute env value
    <*> pure l
substitute env (Case scrutinee clauses l) =
  Case
    <$> substitute env scrutinee
    <*> mapM (substituteClause env) clauses
    <*> pure l
substitute env (Subst body table) = do
  (binderRenaming, innerEnv) <- underBinders env (map fst table) (freeVarsT body)
  Subst
    <$> substitute innerEnv body
    <*> mapM (\(x, e) -> (,) (renameName binderRenaming x) <$> substitute env e) table
substitute _ e@EHole {} = pure e

substituteChain :: (Fresh m) => SubstEnv -> Chain -> m Chain
substituteChain env (Pure e) = Pure <$> substitute env e
substituteChain env (More chain operator t e) =
  More <$> substituteChain env chain <*> pure operator <*> pure t <*> substitute env e

-- | A clause's pattern binds over its body only, never over the scrutinee.
substituteClause :: (Fresh m) => SubstEnv -> CaseClause -> m CaseClause
substituteClause env (CaseClause pattern' body) = do
  (binderRenaming, innerEnv) <-
    underBinders env (extractBinder pattern') (freeVarsT body)
  CaseClause (renamePattern binderRenaming pattern') <$> substitute innerEnv body

-- | Handle a 'Var' or 'Const' occurrence according to the environment:
--
--   * No entry: rebuild the occurrence unchanged.
--
--     @
--     Var x t l   -> Var x t l
--     Const x t l -> Const x t l
--     @
--
--   * 'RenameTo' @x'@: change only the name, preserving the constructor,
--     type, and source ranges.
--
--     @
--     Var x t l   -> Var x' t l
--     Const x t l -> Const x' t l
--     @
--
--   * 'ReplaceWith' @e@: replace the whole occurrence with @e@.
occurrence ::
  SubstEnv ->
  (Name -> Type -> Maybe Range -> Expr) ->
  Name ->
  Type ->
  Maybe Range ->
  Expr
occurrence env build name@(Name text range) t l =
  case lookup text env of
    Nothing -> build name t l
    Just (RenameTo text') -> build (Name text' range) t l
    Just (ReplaceWith e) -> e

-- | Prepare to enter a binder's scope from the outer environment, given the
--   names it binds and the free names of the region, before those binders are
--   subtracted. Returns the binder renaming for the caller to apply and the
--   environment for traversing the bound region. This function does not
--   rewrite AST nodes itself.
--
--   Binder names must be distinct. Type inference enforces this for source
--   ASTs; otherwise a renaming cannot distinguish equal binder names.
underBinders :: (Fresh m) => SubstEnv -> [Name] -> Set Text -> m ([(Text, Text)], SubstEnv)
underBinders env binders freeVarsBeforeBinding = do
  binderRenaming <- allocate forbidden clashing
  pure
    ( binderRenaming,
      [(x, RenameTo x') | (x, x') <- binderRenaming] <> visible
    )
  where
    bound = map nameToText binders

    -- A binder hides its own name for the whole of its scope, and an entry
    -- that names nothing free in that scope cannot do anything there.
    visible =
      filter (\(x, _) -> x `notElem` bound && x `Set.member` freeVarsBeforeBinding) env

    -- What entering this scope would carry in. A binder spelled the same way
    -- would capture it, so that binder has to move.
    incoming = foldMap (carriedIn . snd) visible

    clashing = filter ((`Set.member` incoming) . nameToText) binders
    forbidden = incoming <> freeVarsBeforeBinding <> Set.fromList bound

allocate :: (Fresh m) => Set Text -> [Name] -> m [(Text, Text)]
allocate _ [] = pure []
allocate forbidden (binder : rest) = do
  target <- freshFor forbidden binder
  ((nameToText binder, target) :) <$> allocate (Set.insert target forbidden) rest

-- | A name outside @forbidden@. 'Fresh' only proposes a candidate; whether it
--   is actually unused is checked here, because @Fresh WP@ avoids only the
--   names in its reader scopes and hands back the prefix unchanged for
--   anything else. A clash extends the prefix rather than asking again: that
--   instance is reader-only and would answer identically forever.
freshFor :: (Fresh m) => Set Text -> Name -> m Text
freshFor forbidden binder = go (nameToText binder)
  where
    go prefix = do
      candidate <- freshPre prefix
      if candidate `Set.member` forbidden
        then go (Text.snoc prefix '\'')
        else pure candidate

renameName :: [(Text, Text)] -> Name -> Name
renameName renaming name@(Name text range) =
  case lookup text renaming of
    Nothing -> name
    Just text' -> Name text' range

renamePattern :: [(Text, Text)] -> Pattern -> Pattern
renamePattern _ pattern'@PattLit {} = pattern'
renamePattern renaming (PattBinder x) = PattBinder (renameName renaming x)
renamePattern _ pattern'@PattWildcard {} = pattern'
renamePattern renaming (PattTuple patterns) =
  PattTuple (map (renamePattern renaming) patterns)
-- The constructor names a data constructor, not a binder.
renamePattern renaming (PattConstructor constructor patterns) =
  PattConstructor constructor (map (renamePattern renaming) patterns)
