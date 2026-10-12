-- | Focused reduction over resolved local/global identities. Leading lambdas
-- exposed by unfolding are repaired while rebuilding the focused path. Pending
-- state operations remain separate steps unless consuming them is necessary
-- to avoid exposing a Lam; no whole-tree preparation is necessary.
-- Input predicates are lambda-free. Definitions may have a leading lambda
-- prefix with a lambda-free body; unsaturated unfolds are unavailable.
-- Discovery probes execution using the shared public Expr paths.
module Syntax.Typed.Reduce3
  ( DefnEnv,
    RedexPath,
    RTree (..),
    RT,
    tryReduceAt,
    redexRT,
    pathChildren,
    childBinders,
    withPathChildren,
  )
where

import Control.Monad.State (evalState)
import Data.List ((!?))
import Data.Maybe (isJust)
import GCL.Common (Fresh, initFreshState)
import Syntax.Abstract.Types (Pattern (..))
import Syntax.Common.Types (Name)
import Syntax.Typed.Structure
import Syntax.Typed.Subst3
import Syntax.Typed.Types

type Frame = (Int, Term)

tryReduceAt :: (Fresh m) => DefnEnv -> Expr -> RedexPath -> m (Maybe Expr)
tryReduceAt env expression path = reduceAt (definitions env) expression path

reduceAt :: (Fresh m) => Definitions -> Expr -> RedexPath -> m (Maybe Expr)
reduceAt context expression path =
  case focus (resolve context expression) path of
    Nothing -> pure Nothing
    Just (selected, frames) -> do
      candidate <- case selected of
        Reference {} -> do
          body <- unfold context selected
          case body of
            Nothing -> pure Nothing
            Just result -> repair context result frames
        _ -> fmap (\replacement -> rebuild replacement frames) <$> localStep context selected
      result <- traverse (export context) candidate
      -- A pending environment is unfolded through its reference. Do not mark
      -- a table whose only effect would be to recreate the displayed node.
      pure $ case selected of
        Syntax Subst {} _ | result == Just expression -> Nothing
        _ -> result

-- Consume just the lambda prefix and any environments interposed before its
-- applications. Once lambda-free, stop: surrounding state operations retain
-- their own clicks. Applying state before beta also keeps the argument in the
-- caller's state, e.g. (Inv[x := 5]) x = 5 < x when Inv t = x < t.
repair :: (Fresh m) => Definitions -> Term -> [Frame] -> m (Maybe Term)
repair context (Syntax (Subst _ assignments) (subject : values)) frames
  | leadingLambda subject = do
      result <- substituteState context (zip (map fst assignments) values) subject
      repair context result frames
repair context replacement ((0, Syntax (Subst _ assignments) (_ : values)) : frames)
  | leadingLambda replacement = do
      result <- substituteState context (zip (map fst assignments) values) replacement
      repair context result frames
repair context (Syntax (Lam name _ _ _) [body]) ((0, Syntax App {} [_, argument]) : frames) = do
  result <- substituteLocal context [(name, argument)] body
  repair context result frames
repair _ (Syntax Lam {} _) _ = pure Nothing
repair _ replacement frames = pure (Just (rebuild replacement frames))

leadingLambda :: Term -> Bool
leadingLambda (Syntax Lam {} _) = True
leadingLambda (Syntax Subst {} (subject : _)) = leadingLambda subject
leadingLambda _ = False

focus :: Term -> RedexPath -> Maybe (Term, [Frame])
focus expression [] = Just (expression, [])
focus expression (index : rest) = do
  child <- termChildren expression !? index
  (selected, frames) <- focus child rest
  pure (selected, frames <> [(index, expression)])

rebuild :: Term -> [Frame] -> Term
rebuild = foldl plug
  where
    plug replacement (index, parent) =
      case replaceAt index replacement (termChildren parent) of
        Just children -> withTermChildren parent children
        Nothing -> error "Reduce3: invalid saved frame"

localStep :: (Fresh m) => Definitions -> Term -> m (Maybe Term)
localStep _ (Syntax (OutT index _) [Syntax Tuple {} elements]) = pure (elements !? index)
localStep context (Syntax (Subst _ assignments) (subject : values)) =
  Just <$> substituteState context (zip (map fst assignments) values) subject
localStep context (Syntax (Case _ clauses _) (scrutinee : bodies')) = choose (zip clauses bodies')
  where
    choose [] = pure Nothing
    choose ((CaseClause pattern' _, body) : rest) = case match scrutinee pattern' of
      Matched assignments -> Just <$> substituteLocal context assignments body
      Refuted -> choose rest
      Undecided -> pure Nothing
localStep _ _ = pure Nothing

data Match = Matched [(Name, Term)] | Refuted | Undecided

match :: Term -> Pattern -> Match
match expression (PattBinder name) = Matched [(name, expression)]
match _ (PattWildcard _) = Matched []
match (Syntax (Lit literal _ _) _) (PattLit expected)
  | literal == expected = Matched []
  | otherwise = Refuted
match _ (PattLit _) = Undecided
match (Syntax Tuple {} elements) (PattTuple patterns)
  | length elements == length patterns = foldr combine (Matched []) (zipWith match elements patterns)
  | otherwise = Refuted
match (Syntax Lit {} _) (PattTuple _) = Refuted
match _ (PattTuple _) = Undecided
match _ (PattConstructor _ _) = Undecided

combine :: Match -> Match -> Match
combine Refuted _ = Refuted
combine _ Refuted = Refuted
combine Undecided _ = Undecided
combine _ Undecided = Undecided
combine (Matched left) (Matched right) = Matched (left <> right)

redexRT :: DefnEnv -> Expr -> RT
redexRT env expression = go [] expression
  where
    context = definitions env
    go path node =
      Node
        (isJust (evalState (reduceAt context expression path) initFreshState))
        (zipWith (\index child -> go (path <> [index]) child) [0 ..] (pathChildren node))
