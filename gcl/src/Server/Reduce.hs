{-# LANGUAGE OverloadedStrings #-}

module Server.Reduce where

import Control.Lens
import Control.Monad.State (evalState)
import Data.List (find)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Common (initFreshState)
import GCL.Predicate (PO (..), Spec (..))
import Pretty.Error ()
import Prettyprinter (layoutCompact, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Server.FileState (FileState (..))
import Server.Monad
  ( ServerM,
    getFileState,
    getPendingEdit,
    logTextLn,
    sendWindowInfoMessage,
  )
import Server.Notification.Update (setAndSendFileState)
import Server.Reduce.Types (ReduceTarget (..))
import Syntax.Common.Types (Name)
import Syntax.Typed.Reduce4 (DefnEnv, RedexPath, tryReduceAt)
import qualified Syntax.Typed.Types as T

evalReduce :: DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr
evalReduce defnEnv predicate redex =
  evalState (tryReduceAt defnEnv predicate redex) initFreshState

-- | Apply one display reduction without changing the source-derived conditions.
reduceFileState :: ReduceTarget -> RedexPath -> FileState -> Either [Error] FileState
reduceFileState target redex fs = case target of
  ProofObligation poIndex -> do
    po <- maybe (failure "Proof obligation not found.") Right (fsProofObligations fs ^? element poIndex)
    reduced <- reduceExpr (poReducedPred po)
    return fs {fsProofObligations = fsProofObligations fs & element poIndex .~ po {poReducedPred = reduced}}
  SpecificationPrecondition ident ->
    reduceSpec ident specReducedPreCond (\expr spec -> spec {specReducedPreCond = expr})
  SpecificationPostcondition ident ->
    reduceSpec ident specReducedPostCond (\expr spec -> spec {specReducedPostCond = expr})
  where
    failure message = Left [Others "Reduce" message Nothing]
    reduceExpr expr =
      maybe (failure "Selected expression is not reducible.") Right (evalReduce (fsDefinitions fs) expr redex)
    reduceSpec ident getExpr setExpr = do
      spec <- maybe (failure "Specification not found.") Right (find ((== ident) . specID) (fsSpecifications fs))
      reduced <- reduceExpr (getExpr spec)
      return fs {fsSpecifications = map (\item -> if specID item == ident then setExpr reduced item else item) (fsSpecifications fs)}

reduce :: FilePath -> ReduceTarget -> RedexPath -> ServerM ()
reduce filePath target redex = do
  maybePending <- getPendingEdit filePath
  case maybePending of
    Just _ -> do
      logTextLn "Reduce: pending edit exists, skipping"
      sendWindowInfoMessage "GCL: busy, please retry"
    Nothing -> do
      maybeFs <- getFileState filePath
      let result = do
            fs <- case maybeFs of
              Nothing -> Left [Others "Reduce" "File not loaded." Nothing]
              Just fs -> return fs

            reduceFileState target redex fs

      case result of
        Left errs -> sendWindowInfoMessage (Text.intercalate "\n" $ map (renderStrict . layoutCompact . pretty) errs)
        Right fs' -> do
          setAndSendFileState filePath fs'

collectDefinitions :: T.Program -> [(Name, T.Expr)]
collectDefinitions (T.Program defns _ _ _ _) = concatMap aux defns
  where
    aux T.TypeDefn {} = []
    aux (T.ValDefn name _ expr) = [(name, expr)]
