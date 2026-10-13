{-# LANGUAGE OverloadedStrings #-}

-- | Test runners and output normalisation independent of reducer internals.
module Test.Reduce.Support
  ( DefnEnv,
    ReduceAt,
    MarkRedexes,
    runWith,
    reduciblePathsWith,
    answersWith,
    normalForms,
    loaded,
    prettyExpr,
  )
where

import Control.Monad.State (State, evalState, state)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (initFreshState)
import GCL.Predicate (PO (..))
import Pretty (toText)
import Server.FileState (FileState (..))
import Server.Load (loadAndDig)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Structure
import qualified Syntax.Typed.Types as T

type DefnEnv = [(Name, T.Expr)]

type ReduceAt = DefnEnv -> T.Expr -> RedexPath -> State Int (Maybe T.Expr)

type MarkRedexes = DefnEnv -> T.Expr -> RT

runWith :: ReduceAt -> DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr
runWith reduce env expression path =
  evalState (reduce env expression path) initFreshState

reduciblePathsWith :: MarkRedexes -> DefnEnv -> T.Expr -> [RedexPath]
reduciblePathsWith mark env expression = collect [] (mark env expression)
  where
    collect path (Node reducible children) =
      [path | reducible]
        <> concat
          [ collect (path <> [index]) child
          | (index, child) <- zip [0 ..] children
          ]

-- | Load a program with one PO and explore all marked click orders.
answersWith :: (DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr) -> (DefnEnv -> T.Expr -> [RedexPath]) -> [Text] -> [Text]
answersWith reduce paths sourceLines =
  case loadAndDig "<test>" (Text.unlines sourceLines) of
    Right (_, Right fileState)
      | [po] <- fsProofObligations fileState ->
          normalForms reduce paths (fsDefinitions fileState) (poReducedPred po)
    _ -> error "the test program must load with exactly one proof obligation"

normalForms :: (DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr) -> (DefnEnv -> T.Expr -> [RedexPath]) -> DefnEnv -> T.Expr -> [Text]
normalForms reduce paths defnEnv start = Set.toList (go Set.empty Set.empty [start])
  where
    go _ found [] = found
    go seen found (expression : pending)
      | key `Set.member` seen = go seen found pending
      | Set.size seen > 1000 = error "too many reachable states; a definition may be recursive"
      | null successors = go seen' (Set.insert (normalFormText expression) found) pending
      | otherwise = go seen' found (successors <> pending)
      where
        key = toText expression
        seen' = Set.insert key seen
        successors = map reducePath (paths defnEnv expression)
        reducePath path =
          case reduce defnEnv expression path of
            Just result -> result
            Nothing ->
              error
                ( "a marked redex failed to reduce at path "
                    <> show path
                    <> " in predicate: "
                    <> Text.unpack key
                )

-- | Render with every quantifier binder renamed to @$1@, @$2@, … in order of
--   appearance and runs of spaces collapsed, so that answers differing only
--   in which fresh name a click order drew compare equal.
normalFormText :: T.Expr -> Text
normalFormText expression =
  Text.unwords (Text.words (toText (evalState (canonicalBinders expression) (1 :: Int))))

canonicalBinders :: T.Expr -> State Int T.Expr
canonicalBinders (T.Quant operator binders range body l) = do
  canonical <- mapM (const nextBinder) binders
  let renaming = zip (map (nameToText . fst) binders) canonical
  T.Quant
    <$> canonicalBinders operator
    <*> pure [(Name text r, t) | ((Name _ r, t), text) <- zip binders canonical]
    <*> canonicalBinders (renameFreeNames renaming range)
    <*> canonicalBinders (renameFreeNames renaming body)
    <*> pure l
  where
    nextBinder = state (\k -> ("$" <> Text.pack (show k), k + 1))
canonicalBinders expression =
  withPathChildren expression <$> mapM canonicalBinders (pathChildren expression)

-- | Test-only renaming for quantifier normalisation. Child scopes hide names
-- exactly where their binders apply; the reducer's substitution engine is not
-- used to compare its own results.
renameFreeNames :: [(Text, Text)] -> T.Expr -> T.Expr
renameFreeNames renaming (T.Var name t location) =
  T.Var (rename name) t location
  where
    rename original@(Name text range) =
      maybe original (\target -> Name target range) (lookup text renaming)
renameFreeNames renaming (T.Const name t location) =
  T.Const (rename name) t location
  where
    rename original@(Name text range) =
      maybe original (\target -> Name target range) (lookup text renaming)
renameFreeNames renaming expression =
  withPathChildren expression $
    zipWith under (childBinders expression) (pathChildren expression)
  where
    under binders =
      renameFreeNames (filter (\(key, _) -> key `notElem` map nameToText binders) renaming)

-- | Keep the parser/typechecker/WP pipeline and select the RHS of True => Q.
loaded :: [Text] -> (DefnEnv, T.Expr)
loaded source = case loadAndDig "<test>" (Text.unlines source) of
  Right (_, Right fs)
    | [po] <- fsProofObligations fs,
      T.App (T.App _ _ _) right _ <- poReducedPred po ->
        (fsDefinitions fs, right)
  _ -> error "the test program must load with exactly one implication"

-- | Preserve names when a test checks a particular intermediate step.
prettyExpr :: T.Expr -> Text
prettyExpr = Text.unwords . Text.words . toText
