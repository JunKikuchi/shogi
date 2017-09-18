module ShogiBoardTest.BoardTest.MovesTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import ShogiBoard.Board
import ShogiBoard.Piece hiding (moves)
import ShogiBoard.Square
import ShogiBoard.Color

tests :: TestTree
tests = testGroup "moves"
    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                 歩             R9
    --}
  [ testCase "先手歩兵" $ moves (F5, R9) Black (fromList [((F5, R9), pawn False Black)]) @?= [((F5, R8), False)]
    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R1
    --}
  , testCase "後手歩兵" $ moves (F5, R1) White (fromList [((F5, R1), pawn False White)]) @?= [((F5, R2), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                 金             R8
                 歩             R9
    --}
  , testCase "先手歩兵動けない" $ moves (F5, R9) Black (fromList [((F5, R9), pawn False Black), ((F5, R8), gold Black)]) @?= []

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V金             R8
                 歩             R9
    --}
  , testCase "先手歩兵駒を取る" $ moves (F5, R9) Black (fromList [((F5, R9), pawn False Black), ((F5, R8), gold White)]) @?= [((F5, R8), False)]
  ]
