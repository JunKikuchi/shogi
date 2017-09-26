module ShogiTest (tests) where

import Test.Tasty
import qualified ShogiTest.PositionTest as PositionTest
import qualified ShogiTest.BoardTest as BoardTest
import qualified ShogiTest.PieceTest as PieceTest

tests :: TestTree
tests = testGroup "Shogi"
  [ PositionTest.tests
  , BoardTest.tests
  , PieceTest.tests
  ]