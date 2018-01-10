module ShogiTest.BoardTest.DropsTest (tests) where

import           Shogi.Board
import           Shogi.Color
import           Shogi.Piece      hiding (drops)
import           Shogi.Square
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "drops"
  [ testCase "先手歩兵" $ drops (pawn False Black) (fromList []) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R9]]
  , testCase "後手歩兵" $ drops (pawn False White) (fromList []) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R8]]

  , testCase "先手歩兵二歩回避" $ drops (pawn False Black) (fromList [((file, R9), pawn False Black) | file <- [F1 .. F9]]) @?= []
  , testCase "後手歩兵二歩回避" $ drops (pawn False White) (fromList [((file, R1), pawn False White) | file <- [F1 .. F9]]) @?= []

  , testCase "先手歩兵と金" $ drops (pawn False Black) (fromList [((file, R9), pawn True Black) | file <- [F1 .. F9]]) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R8]]
  , testCase "後手歩兵と金" $ drops (pawn False White) (fromList [((file, R1), pawn True White) | file <- [F1 .. F9]]) @?= [(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R8]]
  ]
