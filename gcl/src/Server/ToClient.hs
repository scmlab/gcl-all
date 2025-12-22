{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module defines types that match the client-side TypeScript types
-- and provides conversion functions from server-side types to client types.
-- The main purpose is to make the conversion from server to client explicit
-- and handle the differences between server (1-based) and client (0-based) ranges.
module Server.ToClient
  ( convertFileStateToJSON,
    FileStateNotification (..),
  )
where

import Data.Aeson (defaultOptions, genericToJSON)
import qualified Data.Aeson as JSON
import Data.Loc.Range (MaybeRanged (..))
import GHC.Generics (Generic)
import qualified Data.Text as Text
import qualified GCL.Predicate as GCL
import qualified GCL.WP.Types as GCL
import qualified Language.LSP.Protocol.Types as LSP
import Pretty.Predicate ()
import Pretty.Typed ()
import Prettyprinter (Pretty (pretty))
import Render.Class (Render (..))
import qualified Server.Monad as Server
import Server.SrcLoc (toLSPRange)

-- | Client-side FileStateNotification type (matches TypeScript FileStateNotification)
-- Sent via gcl/update notification
data FileStateNotification = FileStateNotification
  { filePath :: FilePath,
    specs :: [Specification],
    pos :: [ProofObligation],
    warnings :: [StructWarning]
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Client-side Specification type (matches TypeScript ISpecification)
data Specification = Specification
  { specID :: String,
    preCondition :: String,
    postCondition :: String,
    specRange :: LSP.Range
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Client-side ProofObligation type (matches TypeScript IProofObligation)
data ProofObligation = ProofObligation
  { assumption :: String,
    goal :: String,
    hash :: String,
    proofLocation :: Maybe LSP.Range,
    origin :: POOrigin
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Client-side POOrigin type (matches TypeScript origin type)
data POOrigin = POOrigin
  { tag :: Text.Text,
    location :: Maybe LSP.Range,
    explanation :: Maybe String
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Client-side StructWarning type (matches TypeScript IStructWarning)
data StructWarning
  = MissingBound { range :: LSP.Range }
  deriving stock (Show, Generic)

-- | Convert server-side FileState to JSON for client consumption
-- This function extracts only the fields needed by the client,
-- converts 1-based server ranges to 0-based LSP ranges,
-- and serializes to JSON.
convertFileStateToJSON :: FilePath -> Server.FileState -> JSON.Value
convertFileStateToJSON path serverFileState =
  JSON.toJSON (convertFileStateNotification path serverFileState)

-- | Convert server-side FileState to client-side FileStateNotification
convertFileStateNotification :: FilePath -> Server.FileState -> FileStateNotification
convertFileStateNotification path serverFileState =
  FileStateNotification
    { filePath = path,
      specs = map (convertSpec . Server.unversioned) (Server.specifications serverFileState),
      pos = map (convertPO . Server.unversioned) (Server.proofObligations serverFileState),
      warnings = map (convertWarning . Server.unversioned) (Server.warnings serverFileState)
    }

-- | Convert server-side Spec to client-side Specification
convertSpec :: GCL.Spec -> Specification
convertSpec (GCL.Specification {GCL.specID, GCL.specPreCond, GCL.specPostCond, GCL.specRange}) =
  Specification
    { specID = show specID,
      preCondition = show $ pretty specPreCond,
      postCondition = show $ pretty specPostCond,
      specRange = toLSPRange specRange
    }

-- | Convert server-side PO to client-side ProofObligation
convertPO :: GCL.PO -> ProofObligation
convertPO (GCL.PO {GCL.poPre, GCL.poPost, GCL.poAnchorHash, GCL.poAnchorRange, GCL.poOrigin}) =
  ProofObligation
    { assumption = show $ pretty poPre,
      goal = show $ pretty poPost,
      hash = Text.unpack poAnchorHash,
      proofLocation = fmap toLSPRange poAnchorRange,
      origin = convertOrigin poOrigin
    }

-- | Convert server-side Origin to client-side POOrigin
-- Uses the Render instance to get the tag name and MaybeRanged to get location
convertOrigin :: GCL.Origin -> POOrigin
convertOrigin origin =
  POOrigin
    { tag = Text.pack (show (render origin)),
      location = fmap toLSPRange (maybeRangeOf origin),
      explanation = Nothing
    }

-- | Convert server-side StructWarning to client-side StructWarning
convertWarning :: GCL.StructWarning -> StructWarning
convertWarning (GCL.MissingBound rng) = MissingBound {range = toLSPRange rng}

--------------------------------------------------------------------------------
-- JSON instances for client types

-- Note: FileStateNotification, Specification, ProofObligation,
-- and POOrigin use automatic ToJSON deriving with defaultOptions via 'deriving anyclass'.
-- StructWarning requires a custom instance due to unwrapUnaryRecords = True.

instance JSON.ToJSON StructWarning where
  toJSON :: StructWarning -> JSON.Value
  toJSON = genericToJSON defaultOptions {JSON.unwrapUnaryRecords = True}
