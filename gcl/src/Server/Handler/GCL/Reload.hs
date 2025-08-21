{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.GCL.Reload where

import qualified Data.Aeson.Types as JSON
import Error (Error)
import GCL.Predicate (PO, Spec)
import GHC.Generics (Generic)
import Server.Load (load)
import Server.Monad (FileState (..), ServerM, Versioned, sendDebugMessage)

data ReloadParams = ReloadParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams

instance JSON.ToJSON ReloadParams

handler :: ReloadParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler ReloadParams {filePath} onResult _ = do
  load filePath
  onResult ()
