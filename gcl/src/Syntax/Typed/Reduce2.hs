{-# LANGUAGE OverloadedStrings #-}

-- | One-step expression reduction with lambda repair.
--
-- == Invariants
--
-- * Input 'Expr's and definition 'Env's have passed type checking.  Each
--   definition body has the same type as its references.
--
-- * Expressions passed to 'tryReduceAt' are lambda-free.
--
-- * Concrete syntax cannot introduce lambdas.  A parameterized definition is
--   elaborated as a leading 'Lam' prefix around a lambda-free body.  Unfolding
--   a definition is therefore the only operation that can introduce a 'Lam'
--   into a lambda-free input.
--
-- Given these invariants, a successful 'tryReduceAt' consumes every exposed
-- leading lambda through enclosing function-position applications, so its
-- result is lambda-free.  The invariants are assumed rather than checked
-- dynamically.
--
-- Repair only climbs through 'App' nodes.  A lambda separated from its
-- application by any other node cannot be consumed, so such a path is reported
-- as not clickable even though a reduction exists for it.
--
-- == Reduction contract
--
-- A 'RedexPath' identifies the node selected by the UI.  It is a trigger, not
-- a rewrite boundary: reduction may also rewrite ancestors while consuming
-- applications.  The original path remains the one reported to and received
-- from the UI.
--
-- A successful result means that the selected rule applied and the
-- presentation invariant was satisfied; it does not require the result to
-- differ from the input.
module Syntax.Typed.Reduce2
  ( Env,
    RedexPath,
    RTree (..),
    RT,
    Match (..),
    tryReduceAt,
    smallStep,
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

-- | Definitions visible at a lexical position.
type Env = [(Name, Expr)]

-- | A path from the whole expression to the redex selected by the UI.
type RedexPath = [Int]

-- | A redex tree has exactly the same child shape as 'pathChildren'.
data RTree a = Node a [RTree a]
  deriving (Eq, Show)

type RT = RTree Bool

-- | Try the reduction selected by a path.
--
-- The returned expression is the whole input expression, not merely the
-- replacement for the selected subtree.  A successful unfold may beta-reduce
-- one or more enclosing 'App' nodes while repairing exposed leading lambdas.
--
-- 'Nothing' means that the path is invalid, no reduction rule applies at the
-- selected node, or unfolding exposes a leading lambda that cannot be fully
-- consumed by enclosing applications.  In every case, the path is not
-- clickable.
--
-- Reduction may allocate fresh names while avoiding variable capture.
tryReduceAt :: (Fresh m) => Env -> Expr -> RedexPath -> m (Maybe Expr)
tryReduceAt env whole path =
  case focusAt env whole path of
    Nothing -> pure Nothing
    Just (localEnv, selected) ->
      case selected of
        Var name selectedType _ ->
          case lookup name localEnv of
            Nothing -> pure Nothing
            Just body -> do
              unfolded <- unfoldAt whole path selectedType body
              case unfolded of
                Nothing -> pure Nothing
                Just whole' -> ensureNoLeadingLam whole' path
        _ -> do
          maybeReplacement <- smallStep selected
          case maybeReplacement of
            Nothing -> pure Nothing
            Just replacement -> pure (setAt path replacement whole)

-- | Replace the selected definition reference without capturing free names
-- from its body in binders above the selected path.
--
-- A fresh marker is installed at the path, then ordinary capture-avoiding
-- substitution replaces that marker in the whole expression.  Because the
-- marker occurs exactly once, only binders whose scope contains the selected
-- occurrence can be renamed.
--
-- The marker carries the selected occurrence's type, so the intermediate
-- expression stays well-typed.  Substitution matches the marker by name and
-- never reads that type; it is not worth recomputing from the body, which the
-- first invariant says has the same type anyway.
unfoldAt :: (Fresh m) => Expr -> RedexPath -> Type -> Expr -> m (Maybe Expr)
unfoldAt whole path selectedType body = do
  marker <- freshMarker (allNames whole <> allNames body)
  let markerExpr = Var marker selectedType Nothing
  case setAt path markerExpr whole of
    Nothing -> pure Nothing
    Just holed ->
      Just <$> substExpr [(nameToText marker, body)] holed

freshMarker :: (Fresh m) => Set Text -> m Name
freshMarker forbidden = go "reduce2"
  where
    go prefix = do
      candidate <- freshPre prefix
      if Set.member candidate forbidden
        then go (Text.snoc prefix '\'')
        else pure (Name candidate Nothing)

-- | Repair a leading lambda exposed at @currentPath@.
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
          case focus whole parentPath of
            Just parent@(App (Lam {}) _ _) -> do
              stepped <- betaStep parent
              case stepped >>= \parent' -> setAt parentPath parent' whole of
                Just whole' -> ensureNoLeadingLam whole' parentPath
                Nothing -> pure Nothing
            _ -> pure Nothing
        _ -> pure Nothing
    Just _ -> pure (Just whole)

betaStep :: (Fresh m) => Expr -> m (Maybe Expr)
betaStep (App (Lam binder _ body _) argument _) =
  Just <$> substExpr [(nameToText binder, argument)] body
betaStep _ = pure Nothing

-- | The rules that rewrite a node using only what its own subtree already
-- holds.  Each one moves or copies material that is in place, so it needs
-- neither the environment nor the ancestors, and on a lambda-free input it
-- cannot produce a 'Lam'.  Splicing the result back with 'setAt' is therefore
-- safe on its own.
--
-- The two rules that are /not/ here are the two that break those properties.
-- Unfolding a 'Var' brings in a definition body from outside the tree, so it
-- needs capture avoidance through the ancestors and can expose a 'Lam':
-- 'tryReduceAt' owns it, via 'unfoldAt'.  Beta needs a 'Lam' in the input,
-- which the second invariant forbids, so the only betas performed are the ones
-- 'ensureNoLeadingLam' drives through 'betaStep'.
smallStep :: (Fresh m) => Expr -> m (Maybe Expr)
smallStep (OutT index (Tuple elements)) = pure (atMay index elements)
smallStep (Case scrutinee clauses _) = firstMatch scrutinee clauses
smallStep (Subst subject assignments) =
  Just <$> substExpr (map (first nameToText) assignments) subject
smallStep _ = pure Nothing

-- | The first definitely matching clause.  A refuted clause may be skipped;
-- an undecidable one blocks later clauses because case alternatives are
-- ordered.
firstMatch :: (Fresh m) => Expr -> [CaseClause] -> m (Maybe Expr)
firstMatch _ [] = pure Nothing
firstMatch scrutinee (CaseClause pattern' body : clauses) =
  case matchPattern scrutinee pattern' of
    Matched assignments -> Just <$> substExpr assignments body
    Refuted -> firstMatch scrutinee clauses
    Undecided -> pure Nothing

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
-- Constructor recognition needs constructor names in the environment.  Until
-- Env carries those separately, treating this as undecidable is sound: it
-- blocks the case instead of incorrectly falling through to a catch-all.
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
-- Every probe runs in its own fresh-name state, which is discarded with the
-- candidate result.  Marking therefore cannot affect the names chosen by a
-- later committed reduction.
--
-- Reduction applicability must not depend on the fresh names chosen: fresh
-- allocation may change only a successful result, never whether 'tryReduceAt'
-- returns 'Just' or 'Nothing'.  Every new reduction rule must preserve this
-- invariant.
redexRT :: Env -> Expr -> RT
redexRT env whole = go [] whole
  where
    go path expression =
      Node
        (isJust (evalState (tryReduceAt env whole path) initFreshState))
        ( zipWith
            (\index child -> go (path <> [index]) child)
            [0 ..]
            (pathChildren expression)
        )

reduciblePaths :: Env -> Expr -> [RedexPath]
reduciblePaths env expression = collect [] (redexRT env expression)
  where
    collect path (Node reducible children) =
      [path | reducible]
        <> concat
          [ collect (path <> [index]) child
          | (index, child) <- zip [0 ..] children
          ]

-- | Safely locate a node and the definition environment visible at that node.
focusAt :: Env -> Expr -> RedexPath -> Maybe (Env, Expr)
focusAt env expression [] = Just (env, expression)
focusAt env expression (index : rest) = do
  child <- atMay index (pathChildren expression)
  binders <- atMay index (childBinders expression)
  focusAt (shadowDefinitions binders env) child rest

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

shadowDefinitions :: [Name] -> Env -> Env
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

-- | All term and binder names, used to keep the temporary unfold marker fresh.
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
