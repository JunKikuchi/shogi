module ShogiBoardTest.ListsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import ShogiBoard
import ShogiBoard.Piece
import ShogiBoard.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "move"
  [
    testCase "fromList と toList で相互変換" $ fromLists (toLists shogiBoard) @?= shogiBoard
  ]

shogiBoard :: ShogiBoard
shogiBoard = fromLists ([((F5, R9), king Black), ((F5, R1), king White)], [(pawn False Black), (pawn False White)])
