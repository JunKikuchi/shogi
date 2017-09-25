module ShogiTest (tests) where

import Test.Tasty
import qualified ShogiTest.PieceTest as PieceTest

tests :: TestTree
tests = testGroup "Shogi"
  [ PieceTest.tests
  ]
