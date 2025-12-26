{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines types that match the client-side TypeScript types
-- and provides conversion functions from server-side types to client types.
-- The main purpose is to make the conversion from server to client explicit
-- and handle the differences between server (1-based) and client (0-based) ranges.
module Server.ToClient
  ( toFileStateNotificationJSON,
    toErrorNotificationJSON,
    FileStateNotification (..),
    ErrorNotification (..),
  )
where

import Data.Aeson (defaultOptions, genericToJSON, object, (.=))
import qualified Data.Aeson as JSON
import Data.List.NonEmpty (toList)
import Data.Loc.Range (MaybeRanged (..), Range)
import qualified Data.Text as Text
import qualified Error
import qualified GCL.Predicate as GCL
import qualified GCL.Type as Type
import qualified GCL.WP.Types as GCL
import qualified GCL.WP.Types as WP
import GHC.Generics (Generic)
import qualified Language.LSP.Protocol.Types as LSP
import Pretty.Predicate ()
import Pretty.Typed ()
import Prettyprinter (Pretty (pretty))
import Render.Class (Render (..))
import qualified Server.Monad as Server
import Server.SrcLoc (toLSPPosition, toLSPRange)
import qualified Syntax.Common as Common
import qualified Syntax.Parser.Error as Parse

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
  = MissingBound {range :: LSP.Range}
  deriving stock (Show, Generic)

-- | Convert server-side FileState to JSON for client consumption
-- This function extracts only the fields needed by the client,
-- converts 1-based server ranges to 0-based LSP ranges,
-- and serializes to JSON.
toFileStateNotificationJSON :: FilePath -> Server.FileState -> JSON.Value
toFileStateNotificationJSON path serverFileState =
  JSON.toJSON (toFileStateNotification path serverFileState)

-- | Convert server-side FileState to client-side FileStateNotification
toFileStateNotification :: FilePath -> Server.FileState -> FileStateNotification
toFileStateNotification path serverFileState =
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

--------------------------------------------------------------------------------
-- Error Notification Types

-- | Client-side Name type (matches TypeScript Name interface)
-- Represents a symbol with its source location
data Name = Name
  { symbol :: String,
    location :: Maybe LSP.Range
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Convert server-side Name to client-side Name
convertName :: Common.Name -> Name
convertName (Common.Name text loc) =
  Name
    { symbol = Text.unpack text,
      location = fmap toLSPRange loc
    }

-- | Client-side ErrorNotification type (matches TypeScript ErrorNotification)
-- Sent via gcl/error notification
data ErrorNotification = ErrorNotification
  { filePath :: FilePath,
    errors :: [Error]
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Client-side Error type
data Error
  = ParseError ParseError
  | TypeError TypeError
  | StructError StructError
  | CannotReadFile FilePath
  | Others String String (Maybe LSP.Range)
  deriving stock (Show, Generic)

-- Custom ToJSON instance for Error to match old behavior
-- (different constructors use different field names)
instance JSON.ToJSON Error where
  toJSON (ParseError err) =
    object ["tag" .= JSON.String "ParseError", "message" .= JSON.toJSON err]
  toJSON (TypeError err) =
    object ["tag" .= JSON.String "TypeError", "message" .= JSON.toJSON err]
  toJSON (StructError err) =
    object ["tag" .= JSON.String "StructError", "message" .= JSON.toJSON err]
  toJSON (CannotReadFile fp) =
    object ["tag" .= JSON.String "CannotReadFile", "filePath" .= JSON.toJSON fp]
  toJSON (Others title message loc) =
    object
      [ "tag" .= JSON.String "Others",
        "title" .= JSON.toJSON title,
        "message" .= JSON.toJSON message,
        "location" .= JSON.toJSON loc
      ]

-- | Client-side ParseError
data ParseError
  = LexicalError {position :: LSP.Position}
  | SyntacticError {locatedSymbols :: [LocatedSymbol], message :: String}
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

data LocatedSymbol = LocatedSymbol
  { location :: Maybe LSP.Range,
    symbol :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Convert server-side ParseError to client-side ParseError
convertParseError :: Parse.ParseError -> ParseError
convertParseError (Parse.LexicalError pos) =
  LexicalError {position = toLSPPosition pos}
convertParseError (Parse.SyntacticError syms msg) =
  SyntacticError
    { locatedSymbols = map convertLocatedSymbol (toList syms),
      message = msg
    }

convertLocatedSymbol :: (Maybe Range, String) -> LocatedSymbol
convertLocatedSymbol (rng, s) =
  LocatedSymbol
    { location = fmap toLSPRange rng,
      symbol = s
    }

-- | Client-side TypeError
data TypeError
  = NotInScope {symbol :: Name}
  | UnifyFailed {location :: Maybe LSP.Range, typeExpressions :: [String]}
  | RecursiveType {typeVariable :: Name, typeExpression :: JSON.Value, location :: Maybe LSP.Range}
  | AssignToConst {constSymbol :: Name}
  | UndefinedType {typeVariable :: Name}
  | DuplicatedIdentifiers {identifiers :: [Name]}
  | RedundantNames {names :: [Name]}
  | RedundantExprs {expressions :: [String]}
  | MissingArguments {argumentNames :: [Name]}
  | KindUnifyFailed {location :: Maybe LSP.Range, kindExpressions :: [String]}
  | PatternArityMismatch {location :: Maybe LSP.Range, expected :: Int, received :: Int}
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Convert server-side TypeError to client-side TypeError
convertTypeError :: Type.TypeError -> TypeError
convertTypeError (Type.NotInScope n) =
  NotInScope {symbol = convertName n}
convertTypeError (Type.UnifyFailed t1 t2 l) =
  UnifyFailed
    { location = fmap toLSPRange l,
      typeExpressions = map (show . pretty) [t1, t2]
    }
convertTypeError (Type.RecursiveType n t l) =
  RecursiveType
    { typeVariable = convertName n,
      typeExpression = JSON.toJSON t, -- Preserve old behavior: serialize Type to JSON
      location = fmap toLSPRange l
    }
convertTypeError (Type.AssignToConst n) =
  AssignToConst {constSymbol = convertName n}
convertTypeError (Type.UndefinedType n) =
  UndefinedType {typeVariable = convertName n}
convertTypeError (Type.DuplicatedIdentifiers ns) =
  DuplicatedIdentifiers {identifiers = map convertName ns}
convertTypeError (Type.RedundantNames ns) =
  RedundantNames {names = map convertName ns}
convertTypeError (Type.RedundantExprs es) =
  RedundantExprs {expressions = map (show . pretty) es}
convertTypeError (Type.MissingArguments ns) =
  MissingArguments {argumentNames = map convertName ns}
convertTypeError (Type.KindUnifyFailed k1 k2 l) =
  KindUnifyFailed
    { location = fmap toLSPRange l,
      kindExpressions = map (show . pretty) [k1, k2]
    }
convertTypeError (Type.PatternArityMismatch e a l) =
  PatternArityMismatch
    { location = fmap toLSPRange l,
      expected = e,
      received = a
    }

-- | Client-side StructError
data StructError
  = MissingAssertion {location :: Maybe LSP.Range}
  | MissingPostcondition {location :: Maybe LSP.Range}
  | MultiDimArrayAsgnNotImp {location :: Maybe LSP.Range}
  | LocalVarExceedScope {location :: Maybe LSP.Range}
  deriving stock (Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Convert server-side StructError to client-side StructError
convertStructError :: WP.StructError -> StructError
convertStructError (WP.MissingAssertion l) =
  MissingAssertion {location = fmap toLSPRange l}
convertStructError (WP.MissingPostcondition l) =
  MissingPostcondition {location = fmap toLSPRange l}
convertStructError (WP.MultiDimArrayAsgnNotImp l) =
  MultiDimArrayAsgnNotImp {location = fmap toLSPRange l}
convertStructError (WP.LocalVarExceedScope l) =
  LocalVarExceedScope {location = fmap toLSPRange l}

--------------------------------------------------------------------------------
-- Error Conversion Functions

-- | Convert server-side Error to client-side Error
convertError :: Error.Error -> Error
convertError (Error.ParseError err) = ParseError (convertParseError err)
convertError (Error.TypeError err) = TypeError (convertTypeError err)
convertError (Error.StructError err) = StructError (convertStructError err)
convertError (Error.CannotReadFile fp) = CannotReadFile fp
convertError (Error.Others t m l) = Others t m (fmap toLSPRange l)

-- | Convert server-side errors to ErrorNotification JSON for client
toErrorNotificationJSON :: FilePath -> [Error.Error] -> JSON.Value
toErrorNotificationJSON path errs =
  JSON.toJSON (toErrorNotification path errs)

-- | Convert server-side errors to ErrorNotification for client
toErrorNotification :: FilePath -> [Error.Error] -> ErrorNotification
toErrorNotification path errs =
  ErrorNotification
    { filePath = path,
      errors = map convertError errs
    }
