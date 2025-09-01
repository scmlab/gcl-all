{-# LANGUAGE FlexibleInstances #-}

module Pretty.Common where

import Data.Loc (Loc)
import Pretty.Util
import Prettyprinter (Pretty (pretty))
import Render.Error ()
import Render.Predicate ()
import Render.Syntax.Common ()
import Syntax.Common
import Prelude hiding (Ordering (..))

-- | Name
instance Pretty Name where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc Name where
  prettyWithLoc = fromRenderAndLocated

-- | Operators
instance Pretty (ChainOp Loc) where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc (ChainOp Loc) where
  prettyWithLoc = fromRenderAndLocated

instance Pretty (ArithOp Loc) where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc (ArithOp Loc) where
  prettyWithLoc = fromRenderAndLocated

instance Pretty (TypeOp Loc) where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc (TypeOp Loc) where
  prettyWithLoc = fromRenderAndLocated

instance Pretty (Op Loc) where
  pretty = toDoc . prettyWithLoc

instance PrettyWithLoc (Op Loc) where
  prettyWithLoc = fromRenderAndLocated

-- FIXME: this should work but fix it later

-- -- | Operators
-- instance Pretty a => Pretty (ChainOp a) where
--   pretty = toDoc . prettyWithLoc

-- instance PrettyWithLoc a => PrettyWithLoc (ChainOp a) where
--   prettyWithLoc = fromRenderAndLocated

-- instance Pretty a => Pretty (ArithOp a) where
--   pretty = toDoc . prettyWithLoc

-- instance PrettyWithLoc a => PrettyWithLoc (ArithOp a) where
--   prettyWithLoc = fromRenderAndLocated

-- instance Pretty a => Pretty (TypeOp a) where
--   pretty = toDoc . prettyWithLoc

-- instance PrettyWithLoc a => PrettyWithLoc (TypeOp a) where
--   prettyWithLoc = fromRenderAndLocated

-- instance Pretty a => Pretty (Op a) where
--   pretty = toDoc . prettyWithLoc

-- instance PrettyWithLoc a => PrettyWithLoc (Op a) where
--   prettyWithLoc = fromRenderAndLocated
