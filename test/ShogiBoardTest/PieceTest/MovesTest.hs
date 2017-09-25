module ShogiBoardTest.PieceTest.MovesTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Shogi.Piece
import ShogiBoard.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "moves"
  [ testCase "先手歩兵" $ moves (pawn   False Black) (F5, R9) @?= [[(F5, R8)]]
  , testCase "後手歩兵" $ moves (pawn   False White) (F5, R1) @?= [[(F5, R2)]]
  , testCase "先手香車" $ moves (lance  False Black) (F5, R9) @?= [[(F5, R8), (F5, R7), (F5, R6), (F5, R5), (F5, R4), (F5, R3), (F5, R2), (F5, R1)]]
  , testCase "後手香車" $ moves (lance  False White) (F5, R1) @?= [[(F5, R2), (F5, R3), (F5, R4), (F5, R5), (F5, R6), (F5, R7), (F5, R8), (F5, R9)]]
  , testCase "先手桂馬" $ moves (knight False Black) (F5, R9) @?= [[(F6, R7)], [(F4, R7)]]
  , testCase "後手桂馬" $ moves (knight False White) (F5, R1) @?= [[(F6, R3)], [(F4, R3)]]
  , testCase "先手銀将" $ moves (silver False Black) (F5, R5) @?= [[(F6, R4)], [(F5, R4)], [(F4, R4)], [(F4, R6)], [(F6, R6)]]
  , testCase "後手銀将" $ moves (silver False White) (F5, R5) @?= [[(F6, R6)], [(F5, R6)], [(F4, R6)], [(F4, R4)], [(F6, R4)]]
  , testCase "先手金将" $ moves (gold         Black) (F5, R5) @?= [[(F6, R5)], [(F6, R4)], [(F5, R4)], [(F4, R4)], [(F4, R5)], [(F5, R6)]]
  , testCase "後手金将" $ moves (gold         White) (F5, R5) @?= [[(F6, R5)], [(F6, R6)], [(F5, R6)], [(F4, R6)], [(F4, R5)], [(F5, R4)]]
  , testCase "先手角行" $ moves (bishop False Black) (F5, R5) @?= [[(F6, R4), (F7, R3), (F8, R2), (F9, R1)], [(F4, R4), (F3, R3), (F2, R2), (F1, R1)], [(F4, R6), (F3, R7), (F2, R8), (F1, R9)], [(F6, R6), (F7, R7), (F8, R8), (F9, R9)]]
  , testCase "後手角行" $ moves (bishop False White) (F5, R5) @?= [[(F6, R4), (F7, R3), (F8, R2), (F9, R1)], [(F4, R4), (F3, R3), (F2, R2), (F1, R1)], [(F4, R6), (F3, R7), (F2, R8), (F1, R9)], [(F6, R6), (F7, R7), (F8, R8), (F9, R9)]]
  , testCase "先手飛車" $ moves (rook   False Black) (F5, R5) @?= [[(F6, R5), (F7, R5), (F8, R5), (F9, R5)], [(F5, R4), (F5, R3), (F5, R2), (F5, R1)], [(F4, R5), (F3, R5), (F2, R5), (F1, R5)], [(F5, R6), (F5, R7), (F5, R8), (F5, R9)]]
  , testCase "後手飛車" $ moves (rook   False White) (F5, R5) @?= [[(F6, R5), (F7, R5), (F8, R5), (F9, R5)], [(F5, R4), (F5, R3), (F5, R2), (F5, R1)], [(F4, R5), (F3, R5), (F2, R5), (F1, R5)], [(F5, R6), (F5, R7), (F5, R8), (F5, R9)]]
  , testCase "先手王将" $ moves (king         Black) (F5, R5) @?= [[(F6, R5)], [(F6, R4)], [(F5, R4)], [(F4, R4)], [(F4, R5)], [(F4, R6)], [(F5, R6)], [(F6, R6)]]
  , testCase "後手王将" $ moves (king         White) (F5, R5) @?= [[(F6, R5)], [(F6, R4)], [(F5, R4)], [(F4, R4)], [(F4, R5)], [(F4, R6)], [(F5, R6)], [(F6, R6)]]
  , testCase "先手と金" $ moves (pawn   True  Black) (F5, R5) @?= moves (gold Black) (F5, R5)
  , testCase "後手と金" $ moves (pawn   True  White) (F5, R5) @?= moves (gold White) (F5, R5)
  , testCase "先手成香" $ moves (lance  True  Black) (F5, R5) @?= moves (gold Black) (F5, R5)
  , testCase "後手成香" $ moves (lance  True  White) (F5, R5) @?= moves (gold White) (F5, R5)
  , testCase "先手成桂" $ moves (knight True  Black) (F5, R5) @?= moves (gold Black) (F5, R5)
  , testCase "後手成桂" $ moves (knight True  White) (F5, R5) @?= moves (gold White) (F5, R5)
  , testCase "先手成銀" $ moves (silver True  Black) (F5, R5) @?= moves (gold Black) (F5, R5)
  , testCase "後手成銀" $ moves (silver True  White) (F5, R5) @?= moves (gold White) (F5, R5)
  , testCase "先手龍馬" $ moves (bishop True  Black) (F5, R5) @?= [[(F6, R5)], [(F6, R4), (F7, R3), (F8, R2), (F9, R1)], [(F5, R4)], [(F4, R4), (F3, R3), (F2, R2), (F1, R1)], [(F4, R5)], [(F4, R6), (F3, R7), (F2, R8), (F1, R9)], [(F5, R6)], [(F6, R6), (F7, R7), (F8, R8), (F9, R9)]]
  , testCase "後手龍馬" $ moves (bishop True  White) (F5, R5) @?= [[(F6, R5)], [(F6, R4), (F7, R3), (F8, R2), (F9, R1)], [(F5, R4)], [(F4, R4), (F3, R3), (F2, R2), (F1, R1)], [(F4, R5)], [(F4, R6), (F3, R7), (F2, R8), (F1, R9)], [(F5, R6)], [(F6, R6), (F7, R7), (F8, R8), (F9, R9)]]
  , testCase "先手龍王" $ moves (rook   True  Black) (F5, R5) @?= [[(F6, R5), (F7, R5), (F8, R5), (F9, R5)], [(F6, R4)], [(F5, R4), (F5, R3), (F5, R2), (F5, R1)], [(F4, R4)], [(F4, R5), (F3, R5), (F2, R5), (F1, R5)], [(F4, R6)], [(F5, R6), (F5, R7), (F5, R8), (F5, R9)], [(F6, R6)]]
  , testCase "後手龍王" $ moves (rook   True  White) (F5, R5) @?= [[(F6, R5), (F7, R5), (F8, R5), (F9, R5)], [(F6, R4)], [(F5, R4), (F5, R3), (F5, R2), (F5, R1)], [(F4, R4)], [(F4, R5), (F3, R5), (F2, R5), (F1, R5)], [(F4, R6)], [(F5, R6), (F5, R7), (F5, R8), (F5, R9)], [(F6, R6)]]
  ]
