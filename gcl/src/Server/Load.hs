{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use join" #-}

module Server.Load (LoadResult (..), load) where

import Data.Text (Text)
import qualified Data.Text as Text
import Error (Error (..))
import GCL.Range (Range, posCol, rangeStart)
import qualified GCL.Type as TypeChecking
import qualified GCL.WP as WP
import qualified Language.LSP.Protocol.Types as LSP
import Server.GoToDefn (collectLocationLinks)
import Server.Highlighting (collectHighlighting)
import Server.Hover (collectHoverInfo)
import Server.Monad (FileState (..))
import Server.PositionMapping (idDelta)
import qualified Server.SrcLoc as SrcLoc
import qualified Syntax.Concrete as C
import Syntax.Concrete.Types (GdCmd (..), SepBy (..))
import qualified Syntax.Parser as Parser

data LoadResult
  = LoadError Error
  | LoadNeedsEdit [(Range, Text)]
  | LoadSuccess FileState

-- | Pure load function
load :: FilePath -> Text -> LSP.Int32 -> LoadResult
load filePath source version =
  case Parser.scanAndParse Parser.program filePath source of
    Left err -> LoadError (ParseError err)
    Right concrete ->
      case collectHoles concrete of
        holes@(_ : _) -> LoadNeedsEdit (map (\r -> (r, diggedText r)) holes)
        [] ->
          let abstract = C.toAbstract concrete
           in case TypeChecking.runElaboration abstract mempty of
                Left err -> LoadError (TypeError err)
                Right elaborated ->
                  case WP.sweep elaborated of
                    Left err -> LoadError (StructError err)
                    Right (pos, specs, warnings, _redexes, idCount) ->
                      LoadSuccess
                        FileState
                          { refinedVersion = version,
                            specifications = map (\spec -> (version, spec)) specs,
                            proofObligations = map (\po -> (version, po)) pos,
                            warnings = map (\warning -> (version, warning)) warnings,
                            loadedVersion = version,
                            toOffsetMap = SrcLoc.makeToOffset source,
                            semanticTokens = collectHighlighting concrete,
                            idCount = idCount,
                            definitionLinks = collectLocationLinks abstract,
                            hoverInfos = collectHoverInfo elaborated,
                            positionDelta = idDelta,
                            editedVersion = version
                          }

collectHoles :: C.Program -> [Range]
collectHoles (C.Program _ statements) = collectHolesFromStatements statements
  where
    collectHolesFromStatements :: [C.Stmt] -> [Range]
    collectHolesFromStatements stmts = do
      statement <- stmts
      case statement of
        C.SpecQM range -> [range]
        C.Block _ program _ -> collectHoles program
        C.Do _ commands _ -> collectHolesFromGdCmd commands
        C.If _ commands _ -> collectHolesFromGdCmd commands
        _ -> []

    collectHolesFromGdCmd :: SepBy s C.GdCmd -> [Range]
    collectHolesFromGdCmd s = do
      ranges <- mapSepBy (\(GdCmd _ _ stmts) -> collectHolesFromStatements stmts) s
      ranges

    mapSepBy :: (a -> b) -> SepBy s a -> [b]
    mapSepBy f (Head c) = [f c]
    mapSepBy f (Delim c _ cs) = f c : mapSepBy f cs

-- | Generate the replacement text for a hole
--
-- @
-- some text ?
--
-- some text [!
-- { indent }
-- { indent }!]
-- @
diggedText :: Range -> Text
diggedText range =
  let indent = Text.replicate (posCol (rangeStart range) - 1) " "
   in "[!\n" <> indent <> "\n" <> indent <> "!]"
