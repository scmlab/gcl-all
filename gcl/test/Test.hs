import System.IO (hSetEncoding, stderr, stdin, stdout, utf8)
import qualified Test.Parser as Parser
import qualified Test.Render as Render
import qualified Test.SrcLoc as SrcLoc
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  defaultMain tests

-- TODO: un-un-comment other tests, after fixing parse errors
tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Parser.tests,
      Render.tests,
      SrcLoc.tests
    ]
