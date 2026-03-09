{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Handler.GCL.Refine2 where

import qualified Data.Aeson.Types as JSON
import GCL.Range (mkPos)
import GHC.Generics (Generic)
import Server.Monad (ServerM)
import Server.Refine2 (refine2)

data RefineParams = RefineParams
  { filePath :: FilePath,
    line :: Int, -- 0-based
    character :: Int -- 0-based
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON RefineParams

instance JSON.ToJSON RefineParams

handler :: RefineParams -> (() -> ServerM ()) -> (() -> ServerM ()) -> ServerM ()
handler RefineParams {filePath, line, character} onFinish _ = do
  refine2 filePath (mkPos (line + 1) (character + 1))
  onFinish ()
