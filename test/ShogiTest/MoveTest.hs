module ShogiTest.MoveTest (tests) where

import Test.Tasty
import qualified ShogiTest.MoveTest.MovePieceTest as MovePieceTest
import qualified ShogiTest.MoveTest.DropPieceTest as DropPieceTest

tests :: TestTree
tests = testGroup "move"
  [ MovePieceTest.tests
  , DropPieceTest.tests
  ]
