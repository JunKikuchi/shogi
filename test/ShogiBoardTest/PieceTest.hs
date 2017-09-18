module ShogiBoardTest.PieceTest (tests) where

import Test.Tasty
import qualified ShogiBoardTest.PieceTest.MovesTest      as MovesTest
import qualified ShogiBoardTest.PieceTest.PromotionsTest as PromotionsTest

tests :: TestTree
tests = testGroup "Piece"
  [ MovesTest.tests
  , PromotionsTest.tests
  ]
