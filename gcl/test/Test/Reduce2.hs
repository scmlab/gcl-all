module Test.Reduce2 (tests) where

import qualified Syntax.Typed.Reduce2 as Reduce2
import qualified Test.Reduce as Shared
import Test.Reduce.Support (answersWith, reduciblePathsWith, runWith)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Reduce2"
    [ Shared.basicTests Reduce2.tryReduceAt Reduce2.redexRT,
      Shared.programTests
        (answersWith (runWith Reduce2.tryReduceAt) (reduciblePathsWith Reduce2.redexRT))
    ]
