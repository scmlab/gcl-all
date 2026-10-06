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
-- Repair only climbs through 'App' nodes, so a lambda separated from its
-- application by any other node cannot be consumed, even though a reduction
-- exists for it.
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
    reduciblePaths,
    pathChildren,
    childBinders,
    withPathChildren,
  )
where

import Control.Monad.State (evalState)
import Data.Bifunctor (first)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (Fresh (..), initFreshState)
import Syntax.Abstract.Types (Pattern (..), Type, extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Subst2 (substExpr)
import Syntax.Typed.Types

-- | The definitions visible at a lexical position.
type DefnEnv = [(Name, Expr)]

-- | A path from the whole expression to the redex selected by the UI.
type RedexPath = [Int]

-- | A redex tree has exactly the same child shape as 'pathChildren'.
data RTree a = Node a [RTree a]
  deriving (Eq, Show)

type RT = RTree Bool

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
-- 3. If unfolding exposes a leading 'Lam', use enclosing function-position
--    'App' nodes to beta-reduce until no leading lambda remains; fail if it
--    cannot be fully consumed.
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
tryReduceAt defnEnv whole path =
  case focusAt defnEnv whole path of
    Nothing -> pure Nothing
    Just (localDefnEnv, selected) ->
      case selected of
        Var name selectedType _ ->
          case lookup name localDefnEnv of
            Nothing -> pure Nothing
            Just body -> do
              unfolded <- unfoldAt whole path selectedType body
              case unfolded of
                Nothing -> pure Nothing
                Just whole' -> ensureNoLeadingLam whole' path
        _ -> do
          maybeReplacement <- localStep selected
          case maybeReplacement of
            Nothing -> pure Nothing
            Just replacement -> pure (setAt path replacement whole)

-- | Replace the selected definition reference without capturing free names
-- from its body in binders above the selected path.
--
-- A fresh placeholder variable is installed at the path, then ordinary
-- capture-avoiding substitution replaces that placeholder in the whole
-- expression.  Because the placeholder occurs exactly once in this
-- intermediate expression, only binders whose scope contains the selected
-- occurrence can be renamed.
--
-- The placeholder name only needs to be fresh for @whole@: 'substExpr'
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
      Just <$> substExpr [(nameToText placeholderName, body)] holed

freshPlaceholderName :: (Fresh m) => Set Text -> m Name
freshPlaceholderName forbidden = go "reduce2"
  where
    go prefix = do
      candidate <- freshPre prefix
      if Set.member candidate forbidden
        then go (Text.snoc prefix '\'')
        else pure (Name candidate Nothing)

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
-- Repair can only move upward through an 'App' whose function child is the
-- current lambda.  Every successful repair shortens the path, so this process
-- terminates without a fuel counter.  No subtree search is needed.
ensureNoLeadingLam :: (Fresh m) => Expr -> RedexPath -> m (Maybe Expr)
ensureNoLeadingLam whole currentPath =
  case focus whole currentPath of
    Nothing -> pure Nothing
    Just Lam {} ->
      case unsnoc currentPath of
        Just (parentPath, 0) ->
          -- 0 is an 'App's function child
          case focus whole parentPath of
            Just (App (Lam binder _ body _) argument _) -> do
              reduced <- substExpr [(nameToText binder, argument)] body -- beta
              case setAt parentPath reduced whole of
                Just whole' -> ensureNoLeadingLam whole' parentPath
                Nothing -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing
    Just _ -> pure (Just whole)

-- | Rules that rewrite a node using only its own subtree.  They need neither
-- 'DefnEnv' nor ancestor context.  They introduce no expression structure from
-- outside the subtree, so on a lambda-free input they cannot produce a 'Lam'.
-- Their results can therefore be spliced back with 'setAt' without lambda
-- repair.
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
localStep (OutT index (Tuple elements)) = pure (atMay index elements)
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
    go path expression =
      Node
        (isJust (evalState (tryReduceAt defnEnv whole path) initFreshState))
        ( zipWith
            (\index child -> go (path <> [index]) child)
            [0 ..]
            (pathChildren expression)
        )

reduciblePaths :: DefnEnv -> Expr -> [RedexPath]
reduciblePaths defnEnv expression = collect [] (redexRT defnEnv expression)
  where
    collect path (Node reducible children) =
      [path | reducible]
        <> concat
          [ collect (path <> [index]) child
          | (index, child) <- zip [0 ..] children
          ]

-- | Safely locate a node and the definitions visible at that node.
focusAt :: DefnEnv -> Expr -> RedexPath -> Maybe (DefnEnv, Expr)
focusAt defnEnv expression [] = Just (defnEnv, expression)
focusAt defnEnv expression (index : rest) = do
  child <- atMay index (pathChildren expression)
  binders <- atMay index (childBinders expression)
  focusAt (shadowDefinitions binders defnEnv) child rest

focus :: Expr -> RedexPath -> Maybe Expr
focus expression [] = Just expression
focus expression (index : rest) = do
  child <- atMay index (pathChildren expression)
  focus child rest

setAt :: RedexPath -> Expr -> Expr -> Maybe Expr
setAt [] replacement _ = Just replacement
setAt (index : rest) replacement expression = do
  child <- atMay index children
  child' <- setAt rest replacement child
  children' <- replaceAt index child' children
  pure (withPathChildren expression children')
  where
    children = pathChildren expression

shadowDefinitions :: [Name] -> DefnEnv -> DefnEnv
shadowDefinitions binders =
  filter (\(name, _) -> name `notElem` binders)

-- | The subexpressions into which a reduction path may descend, in path-index
-- order.  This is the single source of truth for path indexing in this module.
pathChildren :: Expr -> [Expr]
pathChildren (Lit _ _ _) = []
pathChildren (Var _ _ _) = []
pathChildren (Const _ _ _) = []
pathChildren (Op _ _) = []
pathChildren (Chain chain) = chainExprs chain
pathChildren (App function argument _) = [function, argument]
pathChildren (Lam _ _ body _) = [body]
pathChildren (Tuple expressions) = expressions
pathChildren (OutT _ expression) = [expression]
pathChildren (Quant operator _ range body _) = [operator, range, body]
pathChildren (ArrIdx array index _) = [array, index]
pathChildren (ArrUpd array index value _) = [array, index, value]
pathChildren (Case scrutinee clauses _) = scrutinee : map clauseBody clauses
  where
    clauseBody (CaseClause _ body) = body
pathChildren (Subst subject assignments) = subject : map snd assignments
pathChildren (EHole _) = []

-- | Binders whose scope covers each corresponding path child.
childBinders :: Expr -> [[Name]]
childBinders (Lam binder _ _ _) = [[binder]]
childBinders (Quant _ binders _ _ _) = [[], names, names]
  where
    names = map fst binders
childBinders (Case _ clauses _) = [] : map clauseBinders clauses
  where
    clauseBinders (CaseClause pattern' _) = extractBinder pattern'
childBinders (Subst _ assignments) =
  map fst assignments : map (const []) assignments
childBinders expression = map (const []) (pathChildren expression)

-- | Rebuild an expression with its path children replaced.  The input list
-- must have the same shape and order as 'pathChildren'.
withPathChildren :: Expr -> [Expr] -> Expr
withPathChildren expression@(Lit _ _ _) [] = expression
withPathChildren expression@(Var _ _ _) [] = expression
withPathChildren expression@(Const _ _ _) [] = expression
withPathChildren expression@(Op _ _) [] = expression
withPathChildren expression@(EHole _) [] = expression
withPathChildren (Chain chain) children
  | length children == length (chainExprs chain) =
      Chain (withChainExprs chain children)
withPathChildren (App _ _ range) [function, argument] =
  App function argument range
withPathChildren (Lam binder t _ range) [body] =
  Lam binder t body range
withPathChildren (Tuple expressions) children
  | length children == length expressions = Tuple children
withPathChildren (OutT index _) [expression] = OutT index expression
withPathChildren (Quant _ binders _ _ range) [operator, restriction, body] =
  Quant operator binders restriction body range
withPathChildren (ArrIdx _ _ range) [array, index] =
  ArrIdx array index range
withPathChildren (ArrUpd _ _ _ range) [array, index, value] =
  ArrUpd array index value range
withPathChildren (Case _ clauses range) (scrutinee : bodies)
  | length bodies == length clauses =
      Case scrutinee (zipWith withBody clauses bodies) range
  where
    withBody (CaseClause pattern' _) body = CaseClause pattern' body
withPathChildren (Subst _ assignments) (subject : values)
  | length values == length assignments =
      Subst subject (zip (map fst assignments) values)
withPathChildren expression children =
  error $
    "withPathChildren: expected "
      <> show (length (pathChildren expression))
      <> " children, given "
      <> show (length children)

-- Chain paths are rightmost first, matching the existing renderer.
chainExprs :: Chain -> [Expr]
chainExprs (Pure expression) = [expression]
chainExprs (More chain _ _ expression) = expression : chainExprs chain

withChainExprs :: Chain -> [Expr] -> Chain
withChainExprs chain expressions
  | length expressions == length (chainExprs chain) = go chain expressions
  | otherwise =
      error $
        "withChainExprs: expected "
          <> show (length (chainExprs chain))
          <> " operands, given "
          <> show (length expressions)
  where
    go (Pure _) [expression] = Pure expression
    go (More chain' operator t _) (expression : rest) =
      More (go chain' rest) operator t expression
    go _ _ = error "withChainExprs: chainExprs and rebuild disagree"

atMay :: Int -> [a] -> Maybe a
atMay index _ | index < 0 = Nothing
atMay _ [] = Nothing
atMay 0 (value : _) = Just value
atMay index (_ : rest) = atMay (index - 1) rest

replaceAt :: Int -> a -> [a] -> Maybe [a]
replaceAt index _ _ | index < 0 = Nothing
replaceAt _ _ [] = Nothing
replaceAt 0 replacement (_ : rest) = Just (replacement : rest)
replaceAt index replacement (value : rest) =
  (value :) <$> replaceAt (index - 1) replacement rest

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [value] = Just ([], value)
unsnoc (value : rest) = first (value :) <$> unsnoc rest

-- | All term and binder names, used to keep the unfold placeholder name fresh.
allNames :: Expr -> Set Text
allNames Lit {} = mempty
allNames (Var name _ _) = Set.singleton (nameToText name)
allNames (Const name _ _) = Set.singleton (nameToText name)
allNames Op {} = mempty
allNames (Chain chain) = allNamesChain chain
allNames (App function argument _) = allNames function <> allNames argument
allNames (Lam binder _ body _) = Set.insert (nameToText binder) (allNames body)
allNames (Tuple expressions) = foldMap allNames expressions
allNames (OutT _ expression) = allNames expression
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
allNames (Subst body assignments) =
  allNames body
    <> Set.fromList (map (nameToText . fst) assignments)
    <> foldMap (allNames . snd) assignments
allNames EHole {} = mempty

allNamesChain :: Chain -> Set Text
allNamesChain (Pure expression) = allNames expression
allNamesChain (More chain _ _ expression) =
  allNamesChain chain <> allNames expression

allNamesClause :: CaseClause -> Set Text
allNamesClause (CaseClause pattern' body) =
  Set.fromList (map nameToText (extractBinder pattern')) <> allNames body
