module GCL.Spec.Util where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Range (Range (Range), extractText, posLine)
import GCL.Spec
import GCL.WP.Types

-- | Return lines within a Spec without indentation
specPayloadWithoutIndentation :: Text -> Spec -> [Text]
specPayloadWithoutIndentation source spec =
  let range@(Range start end) = specRange spec
      spansMultipleLines = posLine start /= posLine end
   in if spansMultipleLines
        then
          let payload = extractText range source
              linesWithIndentation = init $ tail $ Text.lines payload
              splittedIndentedLines = map (Text.break (not . Char.isSpace)) linesWithIndentation
              smallestIndentation = minimum $ map (Text.length . fst) splittedIndentedLines
              trimmedLines = map (\(indentation, content) -> Text.drop smallestIndentation indentation <> content) splittedIndentedLines
           in trimmedLines
        else
          let payload = extractText range source
           in [Text.strip $ Text.drop 2 $ Text.dropEnd 2 payload]
