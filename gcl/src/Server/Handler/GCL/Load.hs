{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Handler.GCL.Load where

import qualified Data.Aeson.Types as JSON
import GHC.Generics (Generic)
import Server.Load (load)
import Server.Monad (ServerM)

data ReloadParams = ReloadParams {filePath :: FilePath}
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReloadParams

instance JSON.ToJSON ReloadParams

handler :: ReloadParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler ReloadParams {filePath} onResult _ = do
  load filePath
  onResult ()
