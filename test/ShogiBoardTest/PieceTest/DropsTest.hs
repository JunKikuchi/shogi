module ShogiBoardTest.PieceTest.DropsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import ShogiBoard.Piece
import ShogiBoard.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "drops"
  [ testCase "先手歩兵" $ drops (pawn   False Black) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R9]]
  , testCase "後手歩兵" $ drops (pawn   False White) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R8]]
  , testCase "先手香車" $ drops (lance  False Black) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R9]]
  , testCase "後手香車" $ drops (lance  False White) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R8]]
  , testCase "先手桂馬" $ drops (knight False Black) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R3 .. R9]]
  , testCase "後手桂馬" $ drops (knight False White) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R7]]
  , testCase "先手銀将" $ drops (silver False Black) @?= allSquares
  , testCase "後手銀将" $ drops (silver False White) @?= allSquares
  , testCase "先手金将" $ drops (gold         Black) @?= allSquares
  , testCase "後手金将" $ drops (gold         White) @?= allSquares
  , testCase "先手角行" $ drops (bishop False Black) @?= allSquares
  , testCase "後手角行" $ drops (bishop False White) @?= allSquares
  , testCase "先手飛車" $ drops (rook   False Black) @?= allSquares
  , testCase "後手飛車" $ drops (rook   False White) @?= allSquares
  ]

allSquares :: [Square]
allSquares = [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R9]]
