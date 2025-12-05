{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.OnDidChangeTextDocument where

import Data.Loc.Range (Range (..), MaybeRanged (..))
import GCL.Predicate (Origin (..), PO (..), Spec (..))
import GCL.WP.Types (StructWarning (MissingBound))
import qualified Language.LSP.Protocol.Types as LSP
import Server.Load (load)
import Server.Monad (FileState (..), ServerM, Versioned, logFileState, logText, modifyFileState, runIfDecreaseDidChangeShouldReload)
import Server.Notification.Update (sendUpdateNotification)
import Server.PositionMapping (PositionDelta, applyChange, mkDelta, toCurrentRange')
import qualified Server.SrcLoc as SrcLoc

handler :: FilePath -> [LSP.TextDocumentContentChangeEvent] -> ServerM ()
handler filePath changes = do
  modifyFileState
    filePath
    ( \filesState@FileState {positionDelta, editedVersion, specifications, proofObligations, warnings} ->
        filesState
          { positionDelta = foldl applyChange positionDelta changes,
            editedVersion = editedVersion + 1,
            specifications = translateThroughOneVersion translateSpecRange editedVersion specifications,
            proofObligations = translateThroughOneVersion translatePoRange editedVersion proofObligations,
            warnings = translateThroughOneVersion translateWarningRange editedVersion warnings
          }
    )
  logFileState filePath (map (\(version, Specification {specRange}) -> (version, specRange)) . specifications)

  runIfDecreaseDidChangeShouldReload filePath load

  -- send notification to update Specs and POs
  logText "didChange: fileState modified\n"
  sendUpdateNotification filePath
  logText "didChange: upate notification sent\n"
  where
    translateThroughOneVersion ::
      (FilePath -> PositionDelta -> a -> Maybe a) ->
      LSP.Int32 ->
      [Versioned a] ->
      [Versioned a]
    translateThroughOneVersion translator fromVersion versioned = do
      (version, a) <- versioned
      let delta :: PositionDelta = mkDelta changes
      if fromVersion == version
        then case translator filePath delta a of
          Nothing -> []
          Just spec' -> [(version + 1, spec')]
        else
          if fromVersion + 1 == version
            then
              [(version, a)]
            else error "should not happen"

-- 目前只維護 specRange，而沒有更新 specPre 和 specPost 裡面的位置資訊
-- 如果未來前端有需要的話，請在這裡維護
translateSpecRange :: FilePath -> PositionDelta -> Spec -> Maybe Spec
translateSpecRange filePath delta spec@Specification {specRange = oldRange} = do
  let oldLspRange :: LSP.Range = SrcLoc.toLSPRange oldRange
  currentLspRange :: LSP.Range <- toCurrentRange' delta oldLspRange
  let newRange = SrcLoc.fromLSPRangeWithoutCharacterOffset filePath currentLspRange
  return $ spec {specRange = newRange}

-- 目前只維護 poOrigin 裡面的 location，而沒有更新 poPre 和 poPost 裡面的位置資訊
-- 如果未來前端有需要的話，請在這裡維護
translatePoRange :: FilePath -> PositionDelta -> PO -> Maybe PO
translatePoRange filePath delta po@PO {poOrigin} = do
  oldRange :: Range <- maybeRangeOf poOrigin
  let oldLspRange :: LSP.Range = SrcLoc.toLSPRange oldRange
  currentLspRange :: LSP.Range <- toCurrentRange' delta oldLspRange
  let newRange = SrcLoc.fromLSPRangeWithoutCharacterOffset filePath currentLspRange
  return $ po {poOrigin = setOriginRange (Just newRange) poOrigin}

translateWarningRange :: FilePath -> PositionDelta -> StructWarning -> Maybe StructWarning
translateWarningRange filePath delta (MissingBound oldRange) = do
  let oldLspRange :: LSP.Range = SrcLoc.toLSPRange oldRange
  currentLspRange :: LSP.Range <- toCurrentRange' delta oldLspRange
  let newRange = SrcLoc.fromLSPRangeWithoutCharacterOffset filePath currentLspRange
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
