module ShogiTest.PieceTest (tests) where

import Test.Tasty
import qualified ShogiTest.PieceTest.MovesTest      as MovesTest
import qualified ShogiTest.PieceTest.DropsTest      as DropsTest
import qualified ShogiTest.PieceTest.PromotionsTest as PromotionsTest

tests :: TestTree
tests = testGroup "Piece"
  [ MovesTest.tests
  , DropsTest.tests
  , PromotionsTest.tests
  ]
