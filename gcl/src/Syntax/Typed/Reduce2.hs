{-# LANGUAGE OverloadedStrings #-}

-- | One-step expression reduction with lambda repair.
--
-- The UI marks the reducible spots in a proof obligation's predicate;
-- clicking one replaces the predicate with the reduced form.
--
-- == Invariants
--
-- * Input 'Expr's and 'DefnEnv' have passed type checking.  Each
--   definition body has the same type as its references.
--
-- * Expressions passed to 'tryReduceAt' are lambda-free.
--
-- * GCL source syntax has no lambda expressions.  A definition with
--   parameters is elaborated as a leading 'Lam' prefix around a lambda-free
--   body.  Unfolding a definition is therefore the only operation that can
--   introduce a 'Lam' into a lambda-free input.
--
-- Given these invariants, 'tryReduceAt' succeeds only when its result is
-- lambda-free: if a 'Lam' would survive, the path is not clickable.  Most
-- reductions cannot produce a 'Lam' at all; an unfold that exposes one
-- consumes it through enclosing function-position applications.
--
-- Repair consumes pending 'Subst' environments and enclosing 'App' nodes.
-- Other nodes still prevent a lambda from reaching its application.
--
-- == Reduction contract
--
-- A 'RedexPath' identifies the node selected by the UI.  It is a trigger, not
-- a rewrite boundary: reduction may also rewrite ancestors while consuming
-- applications.  The original path remains the one reported to and received
-- from the UI.
--
-- A successful result means that the selected rule applied and the
-- presentation invariant was satisfied.
module Syntax.Typed.Reduce2
  ( DefnEnv,
    RedexPath,
    RTree (..),
    RT,
    Match (..),
    tryReduceAt,
    localStep,
    matchPattern,
    redexRT,
    pathChildren,
    childBinders,
    withPathChildren,
  )
where

import Control.Monad.State (evalState)
import Data.Bifunctor (first)
import Data.List (unsnoc, (!?))
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text)
import GCL.Common (Fresh, freshAvoiding, initFreshState)
import Syntax.Abstract.Types (Pattern (..), Type)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Definition
  ( DefinitionContext,
    DefnEnv,
    binderRenaming,
    closeDefinitions,
    definitionContext,
    definitionEnv,
    hiddenGlobals,
  )
import Syntax.Typed.Structure (RT, RTree (..), RedexPath, childBinders, pathChildren, replaceAt, withPathChildren)
import Syntax.Typed.Subst2 (allNames, insertDefinition, renameFree, renameName, renamePatternBinders, substExpr)
import Syntax.Typed.Types

-- | Try the reduction selected by a path.
--
-- 1. Follow the path to find the selected node and the definitions
--    visible there.
--
-- 2. Try to apply one reduction rule:
--
--     * A selected 'Var' unfolds its definition.
--
--     * Any other selected node tries the local 'Case', tuple projection, or
--       explicit substitution rules in 'localStep'.
--
-- 3. If unfolding exposes a leading 'Lam', consume pending substitutions
--    and enclosing function-position applications until no lambda remains.
--
-- 4. Return the complete new 'Expr' on success, or 'Nothing' on failure.
--
-- The path is a reduction trigger, not a rewrite boundary.  Consuming an
-- exposed lambda may therefore also rewrite ancestors of the selected node.
--
-- Failure includes an invalid path, no applicable reduction rule, or a
-- leading lambda that cannot be fully consumed by enclosing applications.
-- A rule that fires is a success; no progress check is made, so the result
-- may equal @whole@.
--
-- Reduction may allocate fresh names while avoiding variable capture.
tryReduceAt :: (Fresh m) => DefnEnv -> Expr -> RedexPath -> m (Maybe Expr)
tryReduceAt defnEnv = tryReduceWith (definitionContext defnEnv)

tryReduceWith :: (Fresh m) => DefinitionContext -> Expr -> RedexPath -> m (Maybe Expr)
tryReduceWith context whole path =
  case focusAt (definitionEnv context) whole path of
    Nothing -> pure Nothing
    Just (localDefnEnv, selected) ->
      case selected of
        Var name selectedType _ ->
          case lookup name localDefnEnv of
            Nothing -> pure Nothing
            Just body -> do
              closed <- closeDefinitions context (definitionEnv context) body
              unfolded <- unfoldAt whole path selectedType closed
              case unfolded of
                Nothing -> pure Nothing
                Just whole' -> ensureNoLeadingLam context whole' path
        Subst {} -> preparedStepAt context whole path
        Case {} -> preparedStepAt context whole path
        OutT _ (Tuple _) -> preparedStepAt context whole path
        _ -> pure Nothing

-- | Replace the selected definition reference without capturing free names
-- from its body in lexical binders above the selected path. Pending program
-- substitutions deliberately capture those globals, so unfolding before or
-- after distributing a substitution observes the same state.
--
-- A fresh placeholder variable is installed at the path, then definition
-- insertion replaces that placeholder in the whole expression. Because the
-- placeholder occurs exactly once in this
-- intermediate expression, only binders whose scope contains the selected
-- occurrence can be renamed.
--
-- The placeholder name only needs to be fresh for @whole@: 'insertDefinition'
-- inserts @body@ as-is and does not traverse replacements.
--
-- The placeholder carries the selected occurrence's type, so the intermediate
-- expression stays well-typed.  Substitution matches the placeholder by name
-- and never reads that type; it is not worth recomputing from the body, which
-- the first invariant says has the same type anyway.
unfoldAt :: (Fresh m) => Expr -> RedexPath -> Type -> Expr -> m (Maybe Expr)
unfoldAt whole path selectedType body = do
  placeholderName <- freshPlaceholderName (allNames whole)
  let placeholder = Var placeholderName selectedType Nothing
  case setAt path placeholder whole of
    Nothing -> pure Nothing
    Just holed ->
      Just <$> insertDefinition placeholderName body holed

freshPlaceholderName :: (Fresh m) => Set Text -> m Name
freshPlaceholderName forbidden =
  Name <$> freshAvoiding forbidden "reduce2" <*> pure Nothing

-- | Repair a leading lambda exposed at @currentPath@.
--
-- For example, before reduction @f a b@ already consists of two nested
-- applications:
--
-- >         App
-- >        /   \
-- >      App    b
-- >     /   \
-- >  Var f   a
--
-- If the selected @Var f@ unfolds to @Lam x (Lam y body)@, the first 'Lam'
-- appears in the inner 'App's function position.  Repair beta-reduces that
-- application with @a@, exposing the next 'Lam' in the outer 'App's function
-- position, which can then beta-reduce with @b@.  Each repair therefore
-- consumes an 'App' node that was already present in the original tree.
--
-- Repair moves upward through a pending substitution's subject or an
-- application's function child. Every repair shortens the path.
ensureNoLeadingLam :: (Fresh m) => DefinitionContext -> Expr -> RedexPath -> m (Maybe Expr)
ensureNoLeadingLam context whole currentPath =
  case focus whole currentPath of
    Nothing -> pure Nothing
    Just Lam {} ->
      case unsnoc currentPath of
        Just (parentPath, 0) ->
          -- 0 is an 'App's function child
          case focus whole parentPath of
            Just (App Lam {} _ _) -> do
              -- Closing the argument exposes globals hidden behind aliases
              -- before beta substitution performs capture avoidance.
              prepared <- prepareAt context whole parentPath
              case prepared of
                Just (whole', _, App (Lam binder _ body _) argument _) -> do
                  reduced <- substExpr [(nameToText binder, argument)] body
                  case setAt parentPath reduced whole' of
                    Just whole'' -> ensureNoLeadingLam context whole'' parentPath
                    Nothing -> pure Nothing
                _ -> pure Nothing
            Just (Subst _ _) -> do
              reduced <- preparedStepAt context whole parentPath
              case reduced of
                Just whole' -> ensureNoLeadingLam context whole' parentPath
                Nothing -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing
    Just _ -> pure (Just whole)

-- | Close references before ordinary substitution, case selection, or tuple
-- projection. A substitution attached directly to a definition stays pending
-- until its reference unfolds; repeatedly returning the same node would make
-- it a useless clickable redex.
preparedStepAt :: (Fresh m) => DefinitionContext -> Expr -> RedexPath -> m (Maybe Expr)
preparedStepAt context whole path = do
  prepared <- prepareAt context whole path
  case prepared of
    Nothing -> pure Nothing
    Just (whole', env, selected) ->
      case selected of
        Subst (Var name _ _) table
          | name `notElem` map fst table, isJust (lookup name env) -> pure Nothing
        _ -> do
          replacement <- localStep selected
          pure (replacement >>= \expression -> setAt path expression whole')

-- | Closing a detached subtree can expose globals that collide with lexical
-- ancestors. Rename those ancestors in the old tree first, preserving its
-- bound occurrences, then introduce the new environments at the same path.
prepareAt :: (Fresh m) => DefinitionContext -> Expr -> RedexPath -> m (Maybe (Expr, DefnEnv, Expr))
prepareAt context whole path =
  case focusAt (definitionEnv context) whole path of
    Nothing -> pure Nothing
    Just (env, selected) -> do
      renamed <- avoidHiddenCaptureAt context (hiddenGlobals context env selected) whole path
      case renamed of
        Nothing -> pure Nothing
        Just whole' ->
          case focusAt (definitionEnv context) whole' path of
            Nothing -> pure Nothing
            Just (env', selected') -> do
              closed <- closeDefinitions context env' selected'
              pure (Just (whole', env', closed))

avoidHiddenCaptureAt :: (Fresh m) => DefinitionContext -> Set Text -> Expr -> RedexPath -> m (Maybe Expr)
avoidHiddenCaptureAt context incoming whole = go whole
  where
    go expression [] = pure (Just expression)
    go expression (index : rest) =
      case childBinders expression !? index of
        Nothing -> pure Nothing
        Just binders -> do
          renaming <- case expression of
            Subst {} -> pure [] -- these keys update the definition's globals
            _ -> binderRenaming context whole binders incoming
          let renamed = renameScoped index renaming expression
              children = pathChildren renamed
          case children !? index of
            Nothing -> pure Nothing
            Just child -> do
              result <- go child rest
              pure $ do
                child' <- result
                children' <- replaceAt index child' children
                pure (withPathChildren renamed children')

-- A quantifier's binders cover both range and body; a case pattern covers
-- only its own clause. Renaming must update all of that scope, not just the
-- path leading to the selected node.
renameScoped :: Int -> [(Text, Text)] -> Expr -> Expr
renameScoped _ renaming (Lam name t body range) =
  Lam (renameName renaming name) t (renameFree renaming body) range
renameScoped _ renaming (Quant operator binders range body location) =
  Quant
    operator
    [(renameName renaming name, t) | (name, t) <- binders]
    (renameFree renaming range)
    (renameFree renaming body)
    location
renameScoped index renaming expression@(Case scrutinee clauses range) =
  case clauses !? (index - 1) of
    Just (CaseClause pattern' body) ->
      case replaceAt (index - 1) (CaseClause (renamePatternBinders renaming pattern') (renameFree renaming body)) clauses of
        Just clauses' -> Case scrutinee clauses' range
        Nothing -> expression
    Nothing -> expression
renameScoped _ _ expression = expression

-- | Rules that rewrite a node using only its own subtree.  They need neither
-- 'DefnEnv' nor ancestor context.  They introduce no expression structure from
-- outside the subtree, so on a lambda-free input they cannot produce a 'Lam'.
-- Their results can therefore be spliced back with 'setAt' without lambda
-- repair.
-- 'tryReduceAt' closes definition references before these rules run, so their
-- ordinary substitution can see the globals hidden inside definitions.
--
-- Unfolding and beta reduction are handled outside 'localStep':
--
-- 1. Unfolding a 'Var' is not subtree-local: it looks up a definition body in
--    'DefnEnv', must avoid capture through ancestors, and can expose a 'Lam'.
--    'tryReduceAt' handles it via 'unfoldAt'.
--
-- 2. Beta reduction is used only for repair.  Although it is subtree-local, a
--    beta-redex contains a 'Lam', so the lambda-free input invariant prevents
--    one from appearing in an input to 'tryReduceAt'.  'ensureNoLeadingLam'
--    performs beta reduction only after unfolding exposes such a redex.
localStep :: (Fresh m) => Expr -> m (Maybe Expr)
localStep (OutT index (Tuple elements)) = pure (elements !? index)
localStep (Case scrutinee clauses _) = selectCase scrutinee clauses
localStep (Subst subject assignments) =
  Just <$> substExpr (map (first nameToText) assignments) subject
localStep _ = pure Nothing

-- | The body of the first definitely matching clause, with its pattern
-- binders substituted.  A refuted clause may be skipped; an undecidable one
-- blocks later clauses because case alternatives are ordered.
selectCase :: (Fresh m) => Expr -> [CaseClause] -> m (Maybe Expr)
selectCase _ [] = pure Nothing
selectCase scrutinee (CaseClause pattern' body : clauses) =
  case matchPattern scrutinee pattern' of
    Matched assignments -> Just <$> substExpr assignments body
    Refuted -> selectCase scrutinee clauses
    Undecided -> pure Nothing

-- | The outcome of matching one pattern against one expression.
--
-- Deliberately three-valued: "does not match" and "cannot determine yet" must
-- remain distinct.  Given a symbolic @x@:
--
-- > case x of
-- >   0 -> a
-- >   _ -> b
--
-- the first pattern is 'Undecided'.  Treating it as 'Refuted' would incorrectly
-- select @b@ through the wildcard without knowing whether @x@ is @0@.
data Match
  = Matched [(Text, Expr)]
  | Refuted
  | Undecided
  deriving (Eq, Show)

matchPattern :: Expr -> Pattern -> Match
matchPattern expression (PattBinder name) =
  Matched [(nameToText name, expression)]
matchPattern _ (PattWildcard _) = Matched []
matchPattern (Lit literal _ _) (PattLit expected)
  | literal == expected = Matched []
  | otherwise = Refuted
matchPattern _ (PattLit _) = Undecided
matchPattern (Tuple expressions) (PattTuple patterns)
  | length expressions == length patterns =
      foldr bothMatch (Matched []) (zipWith matchPattern expressions patterns)
  | otherwise = Refuted
matchPattern Lit {} (PattTuple _) = Refuted
matchPattern _ (PattTuple _) = Undecided
-- Constructor recognition needs a separate constructor environment.  Until
-- one is provided, treating this as undecidable is sound: it blocks the case
-- instead of incorrectly falling through to a catch-all.
matchPattern _ (PattConstructor _ _) = Undecided

bothMatch :: Match -> Match -> Match
bothMatch Refuted _ = Refuted
bothMatch _ Refuted = Refuted
bothMatch Undecided _ = Undecided
bothMatch _ Undecided = Undecided
bothMatch (Matched left) (Matched right) = Matched (left <> right)

-- | Mark every path for which 'tryReduceAt' succeeds.  Marking and execution
-- therefore share the same judgement rather than duplicating can-reduce rules.
--
-- Clickability is decided by trying the reduction and requiring a
-- lambda-free result, not by asking whether the result's type is a function.
-- Those are different questions.  @id inc@ supplies @id@'s only parameter, so
-- no lambda remains, yet the result is function-typed; a type test refuses a
-- reduction whose result, @inc@, is lambda-free and fine to display.
--
-- Every probe runs in its own fresh-name state, which is discarded with the
-- candidate result.  Marking therefore cannot affect the names chosen by a
-- later committed reduction.
--
-- Reduction applicability must not depend on the fresh names chosen: fresh
-- allocation may change only a successful result, never whether 'tryReduceAt'
-- returns 'Just' or 'Nothing'.  Every new reduction rule must preserve this
-- invariant.
redexRT :: DefnEnv -> Expr -> RT
redexRT defnEnv whole = go [] whole
  where
    context = definitionContext defnEnv
    go path expression =
      Node
        (isJust (evalState (tryReduceWith context whole path) initFreshState))
        ( zipWith
            (\index child -> go (path <> [index]) child)
            [0 ..]
            (pathChildren expression)
        )

-- | Safely locate a node and the definitions visible at that node.
focusAt :: DefnEnv -> Expr -> RedexPath -> Maybe (DefnEnv, Expr)
focusAt defnEnv expression [] = Just (defnEnv, expression)
focusAt defnEnv expression (index : rest) = do
  child <- pathChildren expression !? index
  binders <- childBinders expression !? index
  focusAt (shadowDefinitions binders defnEnv) child rest

focus :: Expr -> RedexPath -> Maybe Expr
focus expression [] = Just expression
focus expression (index : rest) = do
  child <- pathChildren expression !? index
  focus child rest

setAt :: RedexPath -> Expr -> Expr -> Maybe Expr
setAt [] replacement _ = Just replacement
setAt (index : rest) replacement expression = do
  child <- children !? index
  child' <- setAt rest replacement child
  children' <- replaceAt index child' children
  pure (withPathChildren expression children')
  where
    children = pathChildren expression

shadowDefinitions :: [Name] -> DefnEnv -> DefnEnv
shadowDefinitions binders =
  filter (\(name, _) -> name `notElem` binders)
