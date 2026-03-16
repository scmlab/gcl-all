module Server.FileState where

import Data.Text (Text)
import GCL.Predicate (Hole, PO, Spec)
import GCL.WP.Types (StructWarning)
import qualified Language.LSP.Protocol.Types as LSP
import Server.GoToDefn (OriginTargetRanges)
import Server.IntervalMap (IntervalMap)

data FileState = FileState
  { fsSpecifications :: ![Spec],
    fsHoles :: ![Hole],
    fsProofObligations :: ![PO],
    fsWarnings :: ![StructWarning],
    fsIdCount :: !Int,
    fsSemanticTokens :: ![LSP.SemanticTokenAbsolute],
    fsDefinitionLinks :: !(IntervalMap OriginTargetRanges),
    fsHoverInfos :: !(IntervalMap LSP.Hover)
  }

data PendingEdit = PendingEdit
  { expectedContent :: !Text,
    pendingFileState :: !FileState
  }

data HoleKind
  = StmtHole
  | ExprHole
  deriving (Eq, Show)
