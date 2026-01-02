{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Pretty.Util
  ( docToText,
    toText,
    docToByteString,
    toByteString,
    docToString,
    toString,
    prefixSpaces,
    PrettyPrec (..),
    PrettyWithRange (..),
    DocWithRange (..),
    toDoc,
    fromDoc,
    fromRender,
    fromRenderPrec,
    fromRenderSection,
    fromRenderAndRanged,
    VList (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GCL.Range
import Prettyprinter
import qualified Prettyprinter.Render.Text as Text
import Render.Class
  ( PrecContext (..),
    Render (..),
    RenderSection (renderSection),
  )
import Prelude hiding (Ordering (..))

-- import           Render

docToText :: Doc ann -> Text
docToText = Text.renderStrict . layoutPretty defaultLayoutOptions

toText :: (Pretty a) => a -> Text
toText = docToText . pretty

docToByteString :: Doc ann -> ByteString
docToByteString = BSL.fromStrict . Text.encodeUtf8 . docToText

toByteString :: (Pretty a) => a -> ByteString
toByteString = docToByteString . pretty

docToString :: Doc ann -> String
docToString = Text.unpack . docToText

toString :: (Pretty a) => a -> String
toString = Text.unpack . toText

--------------------------------------------------------------------------------

-- | Prettifier that respects Locs
data DocWithRange ann
  = -- | A piece of Doc with starting and ending Position
    DocWithRange (Doc ann) Pos Pos
  | -- | As `mempty`
    Empty
  deriving (Show)

-- | Appends two DocWithRange in a srcloc-respecting way
append :: DocWithRange ann -> DocWithRange ann -> DocWithRange ann
append Empty Empty = Empty
append Empty (DocWithRange y c d) = DocWithRange y c d
append (DocWithRange x a b) Empty = DocWithRange x a b
append (DocWithRange x a b) (DocWithRange y c d) =
  if c >= b
    then DocWithRange (x <> fillGap b c <> y) a d
    else DocWithRange (y <> fillGap c b <> x) c b

instance Semigroup (DocWithRange ann) where
  (<>) = append

instance Monoid (DocWithRange ann) where
  mappend = (<>)
  mempty = Empty

fromDoc :: Maybe Range -> Doc ann -> DocWithRange ann
fromDoc Nothing _ = Empty
fromDoc (Just (Range a b)) x = DocWithRange x a b

-- prefixing spaces are ignored before converting to `Doc`
toDoc :: DocWithRange ann -> Doc ann
toDoc (DocWithRange d _ _) = d
toDoc Empty = mempty

prefixSpaces :: DocWithRange ann -> DocWithRange ann
prefixSpaces (DocWithRange d x y) =
  let start = mkPos 1 1
   in DocWithRange (fillGap start x <> d) start y
prefixSpaces Empty = mempty

-- | If something can be rendered, then make it a Doc
fromRender :: (Render a) => a -> Doc ann
fromRender x = pretty (render x)

-- | If something can be rendered with precedence, then make it a Doc
fromRenderPrec :: (Render a) => PrecContext -> a -> Doc ann
fromRenderPrec n x = pretty (renderPrec n x)

-- | If something can be rendered and located, then make it a DocWithRange
fromRenderAndRanged :: (MaybeRanged a, Render a) => a -> DocWithRange ann
fromRenderAndRanged x = case maybeRangeOf x of
  Nothing -> mempty
  Just (Range a b) -> DocWithRange (pretty (render x)) a b

-- | If something can be rendered, then make it a Doc
fromRenderSection :: (RenderSection a) => a -> Doc ann
fromRenderSection x = pretty (renderSection x)

-- generates newlines and spaces to fill the gap between 2 Pos
fillGap :: Pos -> Pos -> Doc ann
fillGap this next =
  let lineDiff = posLine next - posLine this
   in if lineDiff == 0
        then -- on the same line, just pad them with spaces
          let colDiff = posCol next - posCol this
           in mconcat (replicate colDiff space)
        else -- on different lines
          mconcat
            (replicate lineDiff "\n" ++ replicate (posCol next - 1) space)

--------------------------------------------------------------------------------

-- | Pretty print with Precedence
class PrettyPrec a where
  prettyPrec :: PrecContext -> a -> Doc ann

class PrettyWithRange a where
  prettyWithRange :: a -> DocWithRange ann

instance (PrettyWithRange a, PrettyWithRange b) => PrettyWithRange (Either a b) where
  prettyWithRange (Left x) = prettyWithRange x
  prettyWithRange (Right x) = prettyWithRange x

instance (PrettyPrec a, PrettyPrec b) => PrettyPrec (Either a b) where
  prettyPrec i (Left x) = prettyPrec i x
  prettyPrec i (Right x) = prettyPrec i x

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x) = pretty x
  pretty (Right x) = pretty x

instance (PrettyWithRange a) => PrettyWithRange [a] where
  prettyWithRange = mconcat . map prettyWithRange

instance (Pretty a) => PrettyWithRange (R a) where
  prettyWithRange (R range x) = fromDoc (Just range) (pretty x)

--------------------------------------------------------------------------------

-- datatype for printing a list of items vertically without delimiters and enclosings
newtype VList a = VList [a]

instance (Pretty a) => Pretty (VList a) where
  pretty (VList xs) = vcat (map pretty xs)
