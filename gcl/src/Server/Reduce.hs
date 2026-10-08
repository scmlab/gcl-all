{-# LANGUAGE OverloadedStrings #-}

module Server.Reduce where

import Control.Lens
import Control.Monad.State (evalState)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Common (initFreshState)
import GCL.Predicate (PO (..))
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
import Syntax.Common.Types (Name)
import Syntax.Typed.Reduce2 (DefnEnv, RedexPath, tryReduceAt)
import qualified Syntax.Typed.Types as T

evalReduce :: DefnEnv -> T.Expr -> RedexPath -> Maybe T.Expr
evalReduce defnEnv predicate redex =
  evalState (tryReduceAt defnEnv predicate redex) initFreshState

reduce :: FilePath -> Int -> RedexPath -> ServerM ()
reduce filePath poIndex redex = do
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

            let pos = fsProofObligations fs
            let po = pos ^?! element poIndex
            let pred' = poReducedPred po
            reducedPred <-
              case evalReduce (fsDefinitions fs) pred' redex of
                Nothing -> Left [Others "Reduce" "Selected expression is not reducible." Nothing]
                Just result' -> Right result'
            let po' = po {poReducedPred = reducedPred}
            let fs' = fs {fsProofObligations = pos & element poIndex .~ po'}

            return fs'

      case result of
        Left errs -> sendWindowInfoMessage (Text.intercalate "\n" $ map (renderStrict . layoutCompact . pretty) errs)
        Right fs' -> do
          setAndSendFileState filePath fs'

collectDefinitions :: T.Program -> [(Name, T.Expr)]
collectDefinitions (T.Program defns _ _ _ _) = concatMap aux defns
  where
    aux T.TypeDefn {} = []
    aux (T.ValDefn name _ expr) = [(name, expr)]
