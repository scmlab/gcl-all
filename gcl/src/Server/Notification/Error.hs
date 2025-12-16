{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Server.Notification.Error where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Loc.Range (Pos (..), Range (..))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Type (TypeError (..))
import GCL.WP.Types (StructError (..))
import Pretty.Predicate ()
import Pretty.Typed ()
import Prettyprinter (Pretty (pretty))
import Server.Monad (ServerM)
import qualified Server.Monad as Server
import Syntax.Common.Types (Name (..))
import Syntax.Parser.Error (ParseError (..))

sendErrorNotification :: FilePath -> [Error] -> ServerM ()
sendErrorNotification filePath errors = do
  let json :: JSON.Value = makeErrorNotificationJson filePath errors
  Server.sendCustomNotification (Proxy @"gcl/error") json

makeErrorNotificationJson :: FilePath -> [Error] -> JSON.Value
makeErrorNotificationJson filePath errors =
  JSON.object
    [ "filePath" .= JSON.toJSON filePath,
      "errors" .= JSON.toJSON errors
    ]

instance JSON.ToJSON Error where
  toJSON :: Error -> JSON.Value
  toJSON (CannotReadFile filePath) =
    object
      [ "tag" .= JSON.String "CannotReadFile",
        "filePath" .= JSON.toJSON filePath
      ]
  toJSON (ParseError err) =
    object
      [ "tag" .= JSON.String "ParseError",
        "message" .= JSON.toJSON err
      ]
  toJSON (TypeError err) =
    object
      [ "tag" .= JSON.String "TypeError",
        "message" .= JSON.toJSON err
      ]
  toJSON (StructError err) =
    object
      [ "tag" .= JSON.String "StructError",
        "message" .= JSON.toJSON err
      ]
  toJSON (Others title message loc) =
    object
      [ "tag" .= JSON.String "Others",
        "title" .= JSON.toJSON title,
        "message" .= JSON.toJSON message,
        "location" .= fmap toLspRangeJSON loc
      ]

toLspPositionJSON :: Pos -> JSON.Value
toLspPositionJSON (Pos line column _offset) =
  object
    [ "line" .= JSON.toJSON (line - 1),
      "character" .= JSON.toJSON (column - 1)
    ]

-- | Convert a Range to LSP format (0-based line/character)
toLspRangeJSON :: Range -> JSON.Value
toLspRangeJSON (Range start end) =
  object
    [ "start" .= toLspPositionJSON start,
      "end" .= toLspPositionJSON end
    ]

-- | Convert a Name to LSP format JSON
toLspNameJSON :: Name -> JSON.Value
toLspNameJSON (Name text loc) =
  object
    [ "symbol" .= text,
      "location" .= fmap toLspRangeJSON loc
    ]

instance JSON.ToJSON ParseError where
  toJSON :: ParseError -> JSON.Value
  toJSON (LexicalError position) =
    object
      [ "tag" .= JSON.String "LexicalError",
        "position" .= toLspPositionJSON position
      ]
  toJSON (SyntacticError locatedSymbols message) =
    object
      [ "tag" .= JSON.String "SyntacticError",
        "locatedSymbols" .= JSON.toJSON (locatedSymbolsToJSON locatedSymbols),
        "message" .= JSON.toJSON message
      ]
    where
      locatedSymbolsToJSON :: NonEmpty (Maybe Range, String) -> JSON.Value
      locatedSymbolsToJSON (x :| xs) =
        JSON.toJSON $
          map
            ( \(range, s) ->
                object
                  [ "location" .= fmap toLspRangeJSON range,
                    "symbol" .= JSON.toJSON s
                  ]
            )
            (x : xs)

instance JSON.ToJSON TypeError where
  toJSON :: TypeError -> JSON.Value
  toJSON (NotInScope symbol) =
    object
      [ "tag" .= JSON.String "NotInScope",
        "symbol" .= toLspNameJSON symbol
      ]
  toJSON (UnifyFailed type1 type2 loc) =
    object
      [ "tag" .= JSON.String "UnifyFailed",
        "location" .= fmap toLspRangeJSON loc,
        "typeExpressions" .= JSON.toJSON (map (show . pretty) [type1, type2])
      ]
  toJSON (RecursiveType n t loc) =
    object
      [ "tag" .= JSON.String "RecursiveType",
        "typeVariable" .= toLspNameJSON n,
        "typeExpression" .= t,
        "location" .= fmap toLspRangeJSON loc
      ]
  toJSON (AssignToConst name) =
    object
      [ "tag" .= JSON.String "AssignToConst",
        "constSymbol" .= toLspNameJSON name
      ]
  toJSON (UndefinedType name) =
    object
      [ "tag" .= JSON.String "UndefinedType",
        "typeVariable" .= toLspNameJSON name
      ]
  toJSON (DuplicatedIdentifiers names) =
    object
      [ "tag" .= JSON.String "DuplicatedIdentifiers",
        "identifiers" .= JSON.toJSON (map toLspNameJSON names)
      ]
  toJSON (RedundantNames names) =
    object
      [ "tag" .= JSON.String "RedundantNames",
        "names" .= JSON.toJSON (map toLspNameJSON names)
      ]
  toJSON (RedundantExprs expressions) =
    object
      [ "tag" .= JSON.String "RedundantExprs",
        "expressions" .= JSON.toJSON (map (JSON.String . Text.pack . show . pretty) expressions)
      ]
  toJSON (MissingArguments names) =
    object
      [ "tag" .= JSON.String "MissingArguments",
        "argumentNames" .= JSON.toJSON (map toLspNameJSON names)
      ]
  -- FIXME: Implement these.
  toJSON (KindUnifyFailed kind1 kind2 loc) =
    object
      [ "tag" .= JSON.String "KindUnifyFailed",
        "location" .= fmap toLspRangeJSON loc,
        "kindExpressions" .= JSON.toJSON (map (show . pretty) [kind1, kind2])
      ]
  toJSON (PatternArityMismatch expected actual loc) =
    object
      [ "tag" .= JSON.String "PatternArityMismatch",
        "location" .= fmap toLspRangeJSON loc,
        "expected" .= JSON.toJSON expected,
        "received" .= JSON.toJSON actual
      ]

instance JSON.ToJSON StructError where
  toJSON :: StructError -> JSON.Value
  toJSON (MissingAssertion loc) =
    object
      [ "tag" .= JSON.String "MissingAssertion",
        "location" .= fmap toLspRangeJSON loc
      ]
  toJSON (MissingPostcondition loc) =
    object
      [ "tag" .= JSON.String "MissingPostcondition",
        "location" .= fmap toLspRangeJSON loc
      ]
  toJSON (MultiDimArrayAsgnNotImp loc) =
    object
      [ "tag" .= JSON.String "MultiDimArrayAsgnNotImp",
        "location" .= fmap toLspRangeJSON loc
      ]
  toJSON (LocalVarExceedScope loc) =
    object
      [ "tag" .= JSON.String "LocalVarExceedScope",
        "location" .= fmap toLspRangeJSON loc
      ]
