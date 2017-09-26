module ShogiTest.PositionTest (tests) where

import Test.Tasty
import qualified ShogiTest.PositionTest.ListsTest     as ListsTest
import qualified ShogiTest.PositionTest.CheckmateTest as CheckmateTest
import qualified ShogiTest.PositionTest.CheckTest     as CheckTest
import qualified ShogiTest.PositionTest.MoveTest      as MoveTest
import qualified ShogiTest.PositionTest.DropTest      as DropTest
import qualified ShogiTest.PositionTest.MovesTest     as MovesTest
import qualified ShogiTest.PositionTest.DropsTest     as DropsTest


tests :: TestTree
tests = testGroup "Position"
  [ ListsTest.tests
  , CheckmateTest.tests
  , CheckTest.tests
  , MoveTest.tests
  , DropTest.tests
  , MovesTest.tests
  , DropsTest.tests
  ]
