{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Handler.GCL.Reduce where

import qualified Data.Aeson.Types as JSON
import GHC.Generics (Generic)
import Server.Monad (ServerM)
import Server.Reduce (reduce)
import Syntax.Typed.Reduce2 (RedexPath)

data ReduceParams = ReduceParams
  { filePath :: FilePath,
    po :: Int,
    redex :: RedexPath
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReduceParams

instance JSON.ToJSON ReduceParams

handler :: ReduceParams -> ServerM ()
handler ReduceParams {filePath, po, redex} = reduce filePath po redex
