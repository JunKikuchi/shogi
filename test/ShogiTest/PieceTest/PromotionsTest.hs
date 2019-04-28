{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ShogiTest.PieceTest.PromotionsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Shogi.Piece
import Shogi.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "promotions"
  [ unitTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "先手歩兵"      $ promotions (pawn False Black) (F5, R5) (F5, R4) @?= [False]
  , testCase "先手歩兵敵陣内" $ promotions (pawn False Black) (F5, R2) (F5, R1) @?= [True]
  , testCase "先手歩兵敵陣内" $ promotions (pawn False Black) (F5, R3) (F5, R2) @?= [False, True]
  , testCase "先手歩兵敵陣内" $ promotions (pawn False Black) (F5, R4) (F5, R3) @?= [False, True]

  , testCase "後手歩兵"      $ promotions (pawn False White) (F5, R5) (F5, R6) @?= [False]
  , testCase "後手歩兵敵陣内" $ promotions (pawn False White) (F5, R6) (F5, R7) @?= [False, True]
  , testCase "後手歩兵敵陣内" $ promotions (pawn False White) (F5, R7) (F5, R8) @?= [False, True]
  , testCase "後手歩兵敵陣内" $ promotions (pawn False White) (F5, R8) (F5, R9) @?= [True]

  , testCase "先手香車"      $ promotions (lance False Black) (F5, R5) (F5, R4) @?= [False]
  , testCase "先手香車敵陣内" $ promotions (lance False Black) (F5, R2) (F5, R1) @?= [True]
  , testCase "先手香車敵陣内" $ promotions (lance False Black) (F5, R3) (F5, R2) @?= [False, True]
  , testCase "先手香車敵陣内" $ promotions (lance False Black) (F5, R4) (F5, R3) @?= [False, True]

  , testCase "後手香車"      $ promotions (lance False White) (F5, R5) (F5, R6) @?= [False]
  , testCase "後手香車敵陣内" $ promotions (lance False White) (F5, R6) (F5, R7) @?= [False, True]
  , testCase "後手香車敵陣内" $ promotions (lance False White) (F5, R7) (F5, R8) @?= [False, True]
  , testCase "後手香車敵陣内" $ promotions (lance False White) (F5, R8) (F5, R9) @?= [True]

  , testCase "先手桂馬"      $ promotions (knight False Black) (F5, R9) (F4, R7) @?= [False]
  , testCase "先手桂馬敵陣内" $ promotions (knight False Black) (F5, R2) (F5, R1) @?= [True]
  , testCase "先手桂馬敵陣内" $ promotions (knight False Black) (F5, R3) (F5, R2) @?= [True]
  , testCase "先手桂馬敵陣内" $ promotions (knight False Black) (F5, R4) (F5, R3) @?= [False, True]

  , testCase "後手桂馬"      $ promotions (knight False White) (F5, R1) (F6, R3) @?= [False]
  , testCase "後手桂馬敵陣内" $ promotions (knight False White) (F5, R6) (F5, R7) @?= [False, True]
  , testCase "後手桂馬敵陣内" $ promotions (knight False White) (F5, R7) (F5, R8) @?= [True]
  , testCase "後手桂馬敵陣内" $ promotions (knight False White) (F5, R8) (F5, R9) @?= [True]

  , testCase "先手銀将"      $ promotions (silver False Black) (F5, R5) (F5, R4) @?= [False]
  , testCase "先手銀将敵陣内" $ promotions (silver False Black) (F5, R3) (F5, R2) @?= [False, True]
  , testCase "先手銀将敵陣内" $ promotions (silver False Black) (F5, R3) (F4, R4) @?= [False, True]

  , testCase "後手銀将"      $ promotions (silver False White) (F5, R5) (F5, R6) @?= [False]
  , testCase "後手銀将敵陣内" $ promotions (silver False White) (F5, R7) (F5, R8) @?= [False, True]
  , testCase "後手銀将敵陣内" $ promotions (silver False White) (F5, R7) (F4, R6) @?= [False, True]
  ]
