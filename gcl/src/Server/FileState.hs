module Server.FileState where

import Data.Text (Text)
import Error (Error)
import GCL.Predicate (Hole, PO, Spec)
import GCL.WP.Types (StructWarning)
import qualified Language.LSP.Protocol.Types as LSP
import Server.GoToDefn (OriginTargetRanges)
import Server.IntervalMap (IntervalMap)

data FileState = FileState
  { fsErrors :: ![Error],
    fsSpecifications :: ![Spec],
    fsHoles :: ![Hole],
    fsProofObligations :: ![PO],
    fsWarnings :: ![StructWarning],
    fsIdCount :: !Int,
    fsSemanticTokens :: ![LSP.SemanticTokenAbsolute],
    fsDefinitionLinks :: !(IntervalMap OriginTargetRanges),
    fsHoverInfos :: !(IntervalMap LSP.Hover)
  }

emptyFileStateWithErrors :: [Error] -> FileState
emptyFileStateWithErrors errs =
  FileState
    { fsErrors = errs,
      fsSpecifications = [],
      fsHoles = [],
      fsProofObligations = [],
      fsWarnings = [],
      fsIdCount = 0,
      fsSemanticTokens = [],
      fsDefinitionLinks = mempty,
      fsHoverInfos = mempty
    }

data PendingEdit = PendingEdit
  { expectedContent :: !Text,
    pendingFileState :: !FileState
  }

data HoleKind
  = StmtHole
  | ExprHole
  deriving (Eq, Show)
