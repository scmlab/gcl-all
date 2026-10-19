import System.IO (hSetEncoding, stderr, stdin, stdout, utf8)
import qualified Test.Move as Move
import qualified Test.OrigCoord as OrigCoord
import qualified Test.Parser as Parser
import qualified Test.Render as Render
import qualified Test.SrcLoc as SrcLoc
import qualified Test.Subst2 as Subst2
import qualified Test.Subst2Property as Subst2Property
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import qualified Test.Type as Type

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
    [ Move.tests,
      OrigCoord.tests,
      Parser.tests,
      Render.tests,
      SrcLoc.tests,
      Subst2.tests,
      Subst2Property.tests,
      Type.tests
    ]
