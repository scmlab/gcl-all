{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.OnDidChangeTextDocument where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import GCL.Predicate (Hole (..), Origin (..), PO (..), Spec (..))
import GCL.Range (MaybeRanged (..), Range (..))
import GCL.WP.Types (StructWarning (MissingBound))
import GHC.Clock (getMonotonicTimeNSec)
import qualified Language.LSP.Protocol.Types as LSP
import Numeric (showFFloat)
import Server.Change (GCLMove, LSPMove, applyGCLMove, applyLSPMovesToToken, applyMovesToIntervalMap, fromLSPMove, mkLSPMoves, updateOriginTargetRanges)
import Server.Monad (FileState3 (..), PendingEdit (..), ServerM, deletePendingEdit, getFileState3, getPendingEdit, logText, readSource, setFileState3)
import Server.Notification.Update (sendUpdateNotification3)

handler :: FilePath -> [LSP.TextDocumentContentChangeEvent] -> ServerM ()
handler filePath changes = do
  t0 <- liftIO getMonotonicTimeNSec
  maybePending <- getPendingEdit filePath
  case maybePending of
    Just PendingEdit {expectedContent, pendingFileState} -> do
      deletePendingEdit filePath
      maybeSource <- readSource filePath
      case maybeSource of
        Just src | src == expectedContent -> do
          setFileState3 filePath pendingFileState
          sendUpdateNotification3 filePath pendingFileState
        _ -> applyTranslation
    Nothing -> applyTranslation
  t1 <- liftIO getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  logText $ "didChange: took " <> Text.pack (showFFloat (Just 3) elapsedMs "") <> " ms\n"
  where
    applyTranslation = do
      maybeFs3 <- getFileState3 filePath
      case maybeFs3 of
        Nothing -> return ()
        Just fs3 -> do
          let fs3' = translateFileState3 (mkLSPMoves changes) fs3
          setFileState3 filePath fs3'
          sendUpdateNotification3 filePath fs3'

translateFileState3 :: [LSPMove] -> FileState3 -> FileState3
translateFileState3 lspMoves fs3 =
  let gclMoves = map fromLSPMove lspMoves
   in fs3
        { fs3Specifications = mapMaybe (translateSpecRange gclMoves) (fs3Specifications fs3),
          fs3Holes = mapMaybe (translateHoleRange gclMoves) (fs3Holes fs3),
          fs3ProofObligations = mapMaybe (translatePoRange gclMoves) (fs3ProofObligations fs3),
          fs3Warnings = mapMaybe (translateWarningRange gclMoves) (fs3Warnings fs3),
          fs3SemanticTokens = mapMaybe (applyLSPMovesToToken lspMoves) (fs3SemanticTokens fs3),
          fs3DefinitionLinks = applyMovesToIntervalMap gclMoves updateOriginTargetRanges (fs3DefinitionLinks fs3),
          fs3HoverInfos = applyMovesToIntervalMap gclMoves (\_ h -> Just h) (fs3HoverInfos fs3)
        }

-- 目前只維護 specRange，而沒有更新 specPre 和 specPost 裡面的位置資訊
-- 如果未來前端有需要的話，請在這裡維護
translateSpecRange :: [GCLMove] -> Spec -> Maybe Spec
translateSpecRange moves spec@Specification {specRange = oldRange} = do
  newRange <- foldM applyGCLMove oldRange moves
  return $ spec {specRange = newRange}

-- 目前只維護 poOrigin 裡面的 location，而沒有更新 poPre 和 poPost 裡面的位置資訊
-- 如果未來前端有需要的話，請在這裡維護
translatePoRange :: [GCLMove] -> PO -> Maybe PO
translatePoRange moves po@PO {poOrigin} = do
  oldRange :: Range <- maybeRangeOf poOrigin
  newRange <- foldM applyGCLMove oldRange moves
  return $ po {poOrigin = setOriginRange (Just newRange) poOrigin}

-- 目前只維護 holeRange，而沒有更新 holeType 裡面的位置資訊
-- holeType 只會被 pretty print 成字串傳給前端，不會直接用到其中的 Range
-- 如果未來前端有需要的話，請在這裡維護
translateHoleRange :: [GCLMove] -> Hole -> Maybe Hole
translateHoleRange moves hole@Hole {holeRange = oldRange} = do
  newRange <- foldM applyGCLMove oldRange moves
  return $ hole {holeRange = newRange}

translateWarningRange :: [GCLMove] -> StructWarning -> Maybe StructWarning
translateWarningRange moves (MissingBound oldRange) = do
  newRange <- foldM applyGCLMove oldRange moves
  return $ MissingBound newRange

setOriginRange :: Maybe Range -> Origin -> Origin
setOriginRange l (AtAbort _) = AtAbort l
setOriginRange l (AtSkip _) = AtSkip l
setOriginRange l (AtSpec _) = AtSpec l
setOriginRange l (AtAssignment _) = AtAssignment l
setOriginRange l (AtAssertion _) = AtAssertion l
setOriginRange l (AtIf _) = AtIf l
setOriginRange l (AtLoop _) = AtLoop l
setOriginRange l (AtTermination _) = AtTermination l
setOriginRange l (Explain h e i p _) = Explain h e i p l
