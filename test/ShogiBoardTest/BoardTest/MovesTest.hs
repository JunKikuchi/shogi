module ShogiBoardTest.BoardTest.MovesTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Shogi.Board
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
  [ testCase "先手歩兵" $ moves ((F5, R9), Black) (fromList [((F5, R9), pawn False Black)]) @?= [((F5, R8), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R1
                                R2
    --}
  , testCase "後手歩兵" $ moves ((F5, R1), White) (fromList [((F5, R1), pawn False White)]) @?= [((F5, R2), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R7
                 金             R8
                 歩             R9
    --}
  , testCase "先手歩兵動けない" $ moves ((F5, R9), Black) (fromList [((F5, R9), pawn False Black), ((F5, R8), gold Black)]) @?= []

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R7
                V金             R8
                 歩             R9
    --}
  , testCase "先手歩兵駒を取る" $ moves ((F5, R9), Black) (fromList [((F5, R9), pawn False Black), ((F5, R8), gold White)]) @?= [((F5, R8), False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                                R2
                                R3
                 歩             R4
    --}
  , testCase "先手歩兵成れる" $ moves ((F5, R4), Black) (fromList [((F5, R4), pawn False Black)]) @?= [((F5, R3), False), ((F5, R3), True)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R6
                                R7
                                R8
                                R9
    --}
  , testCase "後手手歩兵成れる" $ moves ((F5, R6), White) (fromList [((F5, R6), pawn False White)]) @?= [((F5, R7), False), ((F5, R7), True)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                 歩             R2
    --}
  , testCase "先手歩兵必ず成る" $ moves ((F5, R2), Black) (fromList [((F5, R2), pawn False Black)]) @?= [((F5, R1), True)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R8
                                R9
    --}
  , testCase "後手歩兵必ず成る" $ moves ((F5, R8), White) (fromList [((F5, R8), pawn False White)]) @?= [((F5, R9), True)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                                R2
                                R3
                 と             R4
                                R5
    --}
  , testCase "先手と金" $ moves ((F5, R4), Black) (fromList [((F5, R4), pawn True Black)]) @?= [((F6, R4),False), ((F6, R3),False), ((F5, R3),False), ((F4, R3),False), ((F4, R4),False), ((F5, R5),False)]

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
                 と             R6
                                R7
                                R8
                                R9
    --}
  , testCase "後手と金" $ moves ((F5, R6), White) (fromList [((F5, R6), pawn True White)]) @?= [((F6, R6), False), ((F6, R7), False), ((F5, R7), False),((F4, R7), False), ((F4, R6), False),((F5, R5), False)]
  ]
