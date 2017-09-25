module ShogiTest (tests) where

import Test.Tasty
import qualified ShogiTest.StateTest as StateTest
import qualified ShogiTest.BoardTest as BoardTest
import qualified ShogiTest.PieceTest as PieceTest

tests :: TestTree
tests = testGroup "Shogi"
  [ StateTest.tests
  , BoardTest.tests
  , PieceTest.tests
  ]
