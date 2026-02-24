{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.OnDidChangeTextDocument where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import GCL.Predicate (Origin (..), PO (..), Spec (..))
import GCL.Range (MaybeRanged (..), Range (..))
import GCL.WP.Types (StructWarning (MissingBound))
import GHC.Clock (getMonotonicTimeNSec)
import qualified Language.LSP.Protocol.Types as LSP
import Numeric (showFFloat)
import Server.Change (GCLMove, applyGCLMove, applyLSPMovesToToken, applyMovesToIntervalMap, fromLSPMove, mkLSPMoves, updateOriginTargetRanges)
import Server.Load (load)
import Server.Monad (FileState (..), ServerM, Versioned, logFileState, logText, modifyFileState, runIfDecreaseDidChangeShouldReload)
import Server.Notification.Update (sendUpdateNotification)

handler :: FilePath -> [LSP.TextDocumentContentChangeEvent] -> ServerM ()
handler filePath changes = do
  t0 <- liftIO getMonotonicTimeNSec
  let lspMoves = mkLSPMoves changes
      gclMoves = map fromLSPMove lspMoves
  modifyFileState
    filePath
    ( \filesState@FileState {editedVersion, specifications, proofObligations, warnings, definitionLinks, hoverInfos, semanticTokens} ->
        filesState
          { editedVersion = editedVersion + 1,
            specifications = translateThroughOneVersion (translateSpecRange gclMoves) editedVersion specifications,
            proofObligations = translateThroughOneVersion (translatePoRange gclMoves) editedVersion proofObligations,
            warnings = translateThroughOneVersion (translateWarningRange gclMoves) editedVersion warnings,
            definitionLinks = applyMovesToIntervalMap gclMoves updateOriginTargetRanges definitionLinks,
            hoverInfos = applyMovesToIntervalMap gclMoves (\_ h -> Just h) hoverInfos,
            semanticTokens = mapMaybe (applyLSPMovesToToken lspMoves) semanticTokens
          }
    )
  logFileState filePath (map (\(version, Specification {specRange}) -> (version, specRange)) . specifications)

  runIfDecreaseDidChangeShouldReload filePath load

  t1 <- liftIO getMonotonicTimeNSec
  let elapsedMs = fromIntegral (t1 - t0) / 1e6 :: Double
  logText $ "didChange: took " <> Text.pack (showFFloat (Just 3) elapsedMs "") <> " ms\n"

  -- send notification to update Specs and POs
  logText "didChange: fileState modified\n"
  sendUpdateNotification filePath
  logText "didChange: upate notification sent\n"
  where
    translateThroughOneVersion ::
      (a -> Maybe a) ->
      LSP.Int32 ->
      [Versioned a] ->
      [Versioned a]
    translateThroughOneVersion translator fromVersion versioned = do
      (version, a) <- versioned
      if fromVersion == version
        then case translator a of
          Nothing -> []
          Just a' -> [(version + 1, a')]
        else
          if fromVersion + 1 == version
            then
              [(version, a)]
            else error "should not happen"

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
