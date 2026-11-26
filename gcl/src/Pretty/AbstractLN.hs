module Pretty.AbstractLN where

import Pretty.Common ()
import Pretty.Util
import Prettyprinter
import Render.Class
import Render.Syntax.AbstractLN ()
import Syntax.AbstractLN

-- | Literals
instance Pretty Lit where
  pretty = fromRender

--------------------------------------------------------------------------------

-- | Expressions
instance Pretty Expr where
  pretty = prettyPrec NoContext

instance PrettyPrec Expr where
  prettyPrec = fromRenderPrec

instance Pretty Pattern where
  pretty = prettyPrec NoContext

instance PrettyPrec Pattern where
  prettyPrec = fromRenderPrec
