module ShogiBoardTest.PieceTest (tests) where

import Test.Tasty
import qualified ShogiBoardTest.PieceTest.MovesTest as MovesTest

tests :: TestTree
tests = testGroup "Piece"
  [ MovesTest.tests
  ]
