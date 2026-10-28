{-# LANGUAGE OverloadedStrings #-}

module Server.Config (Config (..), defaultConfig, parseConfig) where

import Data.Aeson (Value, withObject, (.!=), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text

data Config = Config
  { smtTimeoutMs :: Integer,
    smtReductionLimit :: Int
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {smtTimeoutMs = 2000, smtReductionLimit = 100}

-- Missing fields preserve the previous values for clients sending partial updates.
parseConfig :: Config -> Value -> Either Text Config
parseConfig previous = first Text.pack . parseEither (withObject "gcl-vscode settings" parse)
  where
    parse settings =
      Config
        <$> (settings .:? "smtTimeoutMs" .!= smtTimeoutMs previous >>= positiveLimit "smtTimeoutMs")
        <*> (settings .:? "smtReductionLimit" .!= smtReductionLimit previous >>= positiveLimit "smtReductionLimit")

positiveLimit :: (Integral a) => String -> a -> Parser a
positiveLimit name value
  | value >= 1 && value <= 2147483647 = pure value
  | otherwise = fail (name <> " must be an integer between 1 and 2147483647")
