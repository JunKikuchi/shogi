module ShogiTest (tests) where

import Test.Tasty
import qualified ShogiTest.HirateTest    as HirateTest
import qualified ShogiTest.CountdownTest as CountdownTest
import qualified ShogiTest.MoveTest      as MoveTest
import qualified ShogiTest.PositionTest  as PositionTest
import qualified ShogiTest.BoardTest     as BoardTest
import qualified ShogiTest.PieceTest     as PieceTest

tests :: TestTree
tests = testGroup "Shogi"
  [ HirateTest.tests
  , CountdownTest.tests
  , MoveTest.tests
  , PositionTest.tests
  , BoardTest.tests
  , PieceTest.tests
  ]
