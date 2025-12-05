module Pretty.Common where

import Pretty.Util
import Prettyprinter (Pretty (pretty))
import Render.Error ()
import Render.Predicate ()
import Render.Syntax.Common ()
import Syntax.Common
import Prelude hiding (Ordering (..))

-- | Name
instance Pretty Name where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Name where
  prettyWithRange = fromRenderAndRanged

-- | Operators
instance Pretty ChainOp where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange ChainOp where
  prettyWithRange = fromRenderAndRanged

instance Pretty ArithOp where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange ArithOp where
  prettyWithRange = fromRenderAndRanged

instance Pretty TypeOp where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange TypeOp where
  prettyWithRange = fromRenderAndRanged

instance Pretty Op where
  pretty = toDoc . prettyWithRange

instance PrettyWithRange Op where
  prettyWithRange = fromRenderAndRanged
