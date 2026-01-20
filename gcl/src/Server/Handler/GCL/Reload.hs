{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handler.GCL.Reload where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Server.Load (LoadResponse (..), load)
import Server.Monad (ServerM)

data ReloadParams = ReloadParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams

instance JSON.ToJSON ReloadParams

handler :: ReloadParams -> (LoadResponse -> ServerM ()) -> (LoadResponse -> ServerM ()) -> ServerM ()
handler ReloadParams {filePath} onResult _ = do
  response <- load filePath
  onResult response
