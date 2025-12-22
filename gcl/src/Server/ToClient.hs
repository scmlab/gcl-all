{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines types that match the client-side TypeScript types
-- and provides conversion functions from server-side types to client types.
-- The main purpose is to make the conversion from server to client explicit
-- and handle the differences between server (1-based) and client (0-based) ranges.
module Server.ToClient
  ( FileState (..),
    Specification (..),
    ProofObligation (..),
    StructWarning (..),
    POOrigin (..),
    convertFileState,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as JSON
import Data.Loc.Range (MaybeRanged (..))
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

-- | Client-side FileState type (matches TypeScript interface)
data FileState = FileState
  { filePath :: FilePath,
    specs :: [Specification],
    pos :: [ProofObligation],
    warnings :: [StructWarning]
  }
  deriving (Show)

-- | Client-side Specification type (matches TypeScript ISpecification)
data Specification = Specification
  { specID :: String,
    preCondition :: String,
    postCondition :: String,
    specRange :: LSP.Range
  }
  deriving (Show)

-- | Client-side ProofObligation type (matches TypeScript IProofObligation)
data ProofObligation = ProofObligation
  { assumption :: String,
    goal :: String,
    hash :: String,
    proofLocation :: Maybe LSP.Range,
    origin :: POOrigin
  }
  deriving (Show)

-- | Client-side POOrigin type (matches TypeScript origin type)
data POOrigin = POOrigin
  { tag :: Text.Text,
    location :: Maybe LSP.Range,
    explanation :: Maybe String
  }
  deriving (Show)

-- | Client-side StructWarning type (matches TypeScript IStructWarning)
data StructWarning
  = MissingBound LSP.Range
  deriving (Show)

-- | Convert server-side FileState to client-side FileState
-- This function extracts only the fields needed by the client and
-- converts 1-based server ranges to 0-based LSP ranges.
convertFileState :: FilePath -> Server.FileState -> FileState
convertFileState path serverFileState =
  FileState
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
convertWarning (GCL.MissingBound range) = MissingBound (toLSPRange range)

--------------------------------------------------------------------------------
-- JSON instances for client types

instance JSON.ToJSON FileState where
  toJSON :: FileState -> JSON.Value
  toJSON FileState {filePath, specs, pos, warnings} =
    object
      [ "filePath" .= JSON.toJSON filePath,
        "specs" .= JSON.toJSON specs,
        "pos" .= JSON.toJSON pos,
        "warnings" .= JSON.toJSON warnings
      ]

instance JSON.ToJSON Specification where
  toJSON :: Specification -> JSON.Value
  toJSON Specification {specID, preCondition, postCondition, specRange} =
    object
      [ "specID" .= JSON.toJSON specID,
        "preCondition" .= JSON.toJSON preCondition,
        "postCondition" .= JSON.toJSON postCondition,
        "specRange" .= JSON.toJSON specRange
      ]

instance JSON.ToJSON ProofObligation where
  toJSON :: ProofObligation -> JSON.Value
  toJSON ProofObligation {assumption, goal, hash, proofLocation, origin} =
    object
      [ "assumption" .= JSON.toJSON assumption,
        "goal" .= JSON.toJSON goal,
        "hash" .= JSON.toJSON hash,
        "proofLocation" .= case proofLocation of
          Nothing -> JSON.Null
          Just range -> JSON.toJSON range,
        "origin" .= JSON.toJSON origin
      ]

instance JSON.ToJSON POOrigin where
  toJSON :: POOrigin -> JSON.Value
  toJSON POOrigin {tag, location, explanation} =
    object
      [ "tag" .= JSON.String tag,
        "location" .= fmap JSON.toJSON location,
        "explanation" .= fmap JSON.toJSON explanation
      ]

instance JSON.ToJSON StructWarning where
  toJSON :: StructWarning -> JSON.Value
  toJSON (MissingBound range) =
    object
      [ "tag" .= JSON.String "MissingBound",
        "range" .= JSON.toJSON range
      ]
