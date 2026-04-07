{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Server.Handler.GCL.TestVersionedEdit (handler) where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as Text
import Server.Monad
import GCL.Range (mkRange, mkPos)
import qualified Language.LSP.Protocol.Types as LSP

data Params = Params
  { uri :: Text
  } deriving (Generic, JSON.FromJSON)

handler :: Params -> ServerM JSON.Value
handler (Params uriText) = do
  let uri = LSP.Uri uriText
  case LSP.uriToFilePath uri of
    Nothing -> return JSON.Null
    Just filePath -> do
      logTextLn $ "TestVersionedEdit: handling " <> Text.pack filePath
      maybeSourceAndVersion <- readSourceAndVersion filePath
      case maybeSourceAndVersion of
        Nothing -> do
          logTextLn "TestVersionedEdit: source not found"
          return JSON.Null
        Just (source, version) -> do
          let ls = Text.lines source
          case ls of
            [] -> return JSON.Null
            (firstLine : _) -> do
              if "apple" `Text.isInfixOf` firstLine
                then do
                  logTextLn $ "TestVersionedEdit: apple found in version " <> Text.pack (show version)
                  -- Replace the first line (1-based line 1, col 1 to col length+1)
                  let range = mkRange (mkPos 1 1) (mkPos 1 (Text.length firstLine + 1))
                  sendEditTextsWithVersion filePath version [(range, "ZZZZZ")]
                  logTextLn "TestVersionedEdit: edit sent"
                else do
                  logTextLn $ "TestVersionedEdit: apple NOT found in first line: " <> firstLine
              return JSON.Null
