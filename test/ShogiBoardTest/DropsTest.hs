module ShogiBoardTest.DropsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List ((\\))
import ShogiBoard
import Shogi.Piece hiding (drops)
import Shogi.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "drops"
    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                      V王    王 R5
                                R6
     歩
    --}
  [ testCase "先手歩兵" $ drops (pawn False Black) (ShogiBoard.fromLists ([((F1, R5), king Black), ((F3, R5), king White)], [pawn False Black])) @?= ([(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R9]] \\ [(F1, R5), (F3, R5)])

    {--
     V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                       王   V王 R5
                                R6
    --}
  , testCase "後手歩兵" $ drops (pawn False White) (ShogiBoard.fromLists ([((F1, R5), king White), ((F3, R5), king Black)], [pawn False White])) @?= ([(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R8]] \\ [(F1, R5), (F3, R5)])

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V香             R7
                                R8
                 王             R9
     歩
    --}
  , testCase "先手歩兵王手回避" $ drops (pawn False Black) (ShogiBoard.fromLists ([((F5, R7), lance False White), ((F5, R9), king Black)], [(pawn False Black)])) @?= [(F5, R8)]

    {--
     V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
                 香             R3
    --}
  , testCase "後手歩兵王手回避" $ drops (pawn False White) (ShogiBoard.fromLists ([((F5, R1), king White), ((F5, R3), lance False Black)], [(pawn False White)])) @?= [(F5, R2)]
  ]
