module Test.Reduce3 (tests) where

import qualified Syntax.Typed.Reduce3 as Reduce3
import qualified Test.Reduce as Shared
import Test.Tasty (TestTree)

tests :: TestTree
tests = Shared.tests "Reduce3" Reduce3.tryReduceAt Reduce3.redexRT
