module ShogiTest.PositionTest.MovesTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Shogi.Position
import Shogi.Piece hiding (moves)
import Shogi.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "moves"
    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R8
                 歩             R9
    --}
  [ testCase "先手歩兵" $ moves ((F5, R9), Black) (fromLists ([((F5, R9), pawn False Black)], [])) @?= [((F5, R8), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R1
                                R2
    --}
  , testCase "後手歩兵" $ moves ((F5, R1), White) (fromLists ([((F5, R1), pawn False White)], [])) @?= [((F5, R2), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V香             R6
                                R7
                 金             R8
                 王             R9
    --}
  , testCase "先手金将王手回避" $ moves ((F5, R8), Black) (fromLists ([((F5, R6), lance False White), ((F5, R8), gold Black), ((F5, R9), king Black)], [])) @?= [((F5, R7), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                V金             R2
                                R3
                 香             R4
    --}
  , testCase "後手金将王手回避" $ moves ((F5, R2), White) (fromLists ([((F5, R1), king White), ((F5, R2), gold White), ((F5, R4), lance False Black)], [])) @?= [((F5, R3), False)]
  ]
