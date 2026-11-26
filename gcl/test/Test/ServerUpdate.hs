{-# LANGUAGE OverloadedStrings #-}

module Test.ServerUpdate (tests) where

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Text (Text)
import GCL.WP.Types (PO (..))
import qualified Language.LSP.Protocol.Types as LSP
import Server.FileState (FileState (..), emptyFileStateWithErrors)
import Server.Load (loadAndDig)
import Server.Move (applyMovesToFileState, mkLSPMove)
import Server.ToClient (toClientFileStateJSON)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Server updates"
    [ testCase "load notifications identify the analyzed document version" $
        metadata (Just 12) (emptyFileStateWithErrors []) @?= Right ("test.gcl", Just 12),
      testCase "incremental notifications do not claim to recheck proofs" $
        metadata Nothing (emptyFileStateWithErrors []) @?= Right ("test.gcl", Nothing),
      testCase "load detects an inserted proof and clears it after deletion" $ do
        let initial = loaded source
        po <- onlyPO initial
        let withProof = source <> "\n{-#\n" <> poStrippedPred po <> "\n---\n\n#-}\n"
        poProofExists <$> onlyPO (loaded withProof) >>= (@?= True)
        poProofExists <$> onlyPO (loaded source) >>= (@?= False),
      testCase "didChange only translates the previous proof state" $ do
        po <- onlyPO (loaded source)
        let withProof = source <> "\n{-#\n" <> poStrippedPred po <> "\n---\n\n#-}\n"
            fs = loaded withProof
            change = mkLSPMove (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)) "\n"
            moved = applyMovesToFileState [change] fs
        poProofExists <$> onlyPO moved >>= (@?= True)
        metadata Nothing moved @?= Right ("test.gcl", Nothing)
    ]

source :: Text
source = "var x : Int\n{ x = 0 }\nskip\n{ x = 0 }\n"

loaded :: Text -> FileState
loaded text = case loadAndDig "test.gcl" text of
  Right (_, Right fs) -> fs
  _ -> error "test program could not be loaded"

onlyPO :: FileState -> IO PO
onlyPO fs = case fsProofObligations fs of
  [po] -> pure po
  pos -> assertFailure ("expected one PO, got " ++ show (length pos)) >> fail "expected one PO"

metadata :: Maybe LSP.Int32 -> FileState -> Either String (String, Maybe LSP.Int32)
metadata loadDocumentVersion fs = JSON.parseEither parser (toClientFileStateJSON "test.gcl" loadDocumentVersion fs)
  where
    parser = JSON.withObject "gcl/update" $ \obj ->
      (,) <$> obj .: "filePath" <*> obj .: "loadDocumentVersion"
