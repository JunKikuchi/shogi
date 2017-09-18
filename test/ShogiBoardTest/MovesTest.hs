module ShogiBoardTest.MovesTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import ShogiBoard
import ShogiBoard.Board as Board hiding (moves)
import ShogiBoard.Stand as Stand
import ShogiBoard.Piece hiding (moves)
import ShogiBoard.Square
import ShogiBoard.Color

tests :: TestTree
tests = testGroup "moves"
    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R8
                 歩             R9
    --}
  [ testCase "先手歩兵" $ moves (F5, R9) Black (ShogiBoard (Board.fromList [((F5, R9), pawn False Black)]) (Stand.fromList [])) @?= [((F5, R8), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R1
                                R2
    --}
  , testCase "後手歩兵" $ moves (F5, R1) White (ShogiBoard (Board.fromList [((F5, R1), pawn False White)]) (Stand.fromList [])) @?= [((F5, R2), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V香             R6
                                R7
                 金             R8
                 王             R9
    --}
  , testCase "先手金将王手回避" $
      moves (F5, R8) Black (ShogiBoard (Board.fromList [((F5, R6), lance False White), ((F5, R8), gold Black), ((F5, R9), king Black)]) (Stand.fromList [])) @?= [((F5, R7), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                V金             R2
                                R3
                 香             R4
    --}
  , testCase "後手金将王手回避" $
      moves (F5, R2) White (ShogiBoard (Board.fromList [((F5, R1), king White), ((F5, R2), gold White), ((F5, R4), lance False Black)]) (Stand.fromList [])) @?= [((F5, R3), False)]
  ]
