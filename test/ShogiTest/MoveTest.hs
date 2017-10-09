module ShogiTest.MoveTest (tests) where

import Test.Tasty
import qualified ShogiTest.MoveTest.MovePieceTest as MovePieceTest
import qualified ShogiTest.MoveTest.DropPieceTest as DropPieceTest
import qualified ShogiTest.MoveTest.ResignTest    as ResignTest

tests :: TestTree
tests = testGroup "move"
  [ MovePieceTest.tests
  , DropPieceTest.tests
  , ResignTest.tests
  ]
