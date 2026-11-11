{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.GCL.Reduce where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson.Types as JSON
import GHC.Generics (Generic)
import Server.Monad (ServerM)
import Server.Reduce (reduce)
import Server.Reduce.Types (ReduceTarget (..))
import Syntax.Typed.Reduce4 (RedexPath)

data ReduceParams = ReduceParams
  { filePath :: FilePath,
    target :: ReduceTarget,
    redex :: RedexPath
  }
  deriving (Eq, Show, Generic)

instance JSON.FromJSON ReduceParams where
  parseJSON = JSON.withObject "ReduceParams" $ \obj -> do
    maybeTarget <- obj .:? "target"
    -- Preserve the existing {po, redex} request used by PO clicks.
    target <- maybe (ProofObligation <$> obj .: "po") pure maybeTarget
    ReduceParams <$> obj .: "filePath" <*> pure target <*> obj .: "redex"

instance JSON.ToJSON ReduceParams where
  toJSON ReduceParams {filePath, target, redex} =
    JSON.object ["filePath" .= filePath, "target" .= target, "redex" .= redex]

handler :: ReduceParams -> ServerM ()
handler ReduceParams {filePath, target, redex} = reduce filePath target redex
