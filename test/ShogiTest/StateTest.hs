module ShogiTest.StateTest (tests) where

import Test.Tasty
import qualified ShogiTest.StateTest.ListsTest     as ListsTest
import qualified ShogiTest.StateTest.CheckmateTest as CheckmateTest
import qualified ShogiTest.StateTest.CheckTest     as CheckTest
import qualified ShogiTest.StateTest.MoveTest      as MoveTest
import qualified ShogiTest.StateTest.DropTest      as DropTest
import qualified ShogiTest.StateTest.MovesTest     as MovesTest
import qualified ShogiTest.StateTest.DropsTest     as DropsTest


tests :: TestTree
tests = testGroup "State"
  [ ListsTest.tests
  , CheckmateTest.tests
  , CheckTest.tests
  , MoveTest.tests
  , DropTest.tests
  , MovesTest.tests
  , DropsTest.tests
  ]
