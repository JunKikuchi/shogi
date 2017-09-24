{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module ShogiBoardTest.PieceTest.PromotionsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series
import ShogiBoard.Piece
import ShogiBoard.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "promotions"
  [ unitTests
  , smallChecks
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "先手歩兵敵陣内" $ promotions (pawn False Black) (F5, R1) @?= [True]
  , testCase "先手歩兵敵陣内" $ promotions (pawn False Black) (F5, R2) @?= [False, True]
  , testCase "先手歩兵敵陣内" $ promotions (pawn False Black) (F5, R3) @?= [False, True]

  , testCase "後手歩兵敵陣内" $ promotions (pawn False White) (F5, R7) @?= [False, True]
  , testCase "後手歩兵敵陣内" $ promotions (pawn False White) (F5, R8) @?= [False, True]
  , testCase "後手歩兵敵陣内" $ promotions (pawn False White) (F5, R9) @?= [True]

  , testCase "先手香車敵陣内" $ promotions (lance False Black) (F5, R1) @?= [True]
  , testCase "先手香車敵陣内" $ promotions (lance False Black) (F5, R2) @?= [False, True]
  , testCase "先手香車敵陣内" $ promotions (lance False Black) (F5, R3) @?= [False, True]

  , testCase "後手香車敵陣内" $ promotions (lance False White) (F5, R7) @?= [False, True]
  , testCase "後手香車敵陣内" $ promotions (lance False White) (F5, R8) @?= [False, True]
  , testCase "後手香車敵陣内" $ promotions (lance False White) (F5, R9) @?= [True]

  , testCase "先手桂馬敵陣内" $ promotions (knight False Black) (F5, R1) @?= [True]
  , testCase "先手桂馬敵陣内" $ promotions (knight False Black) (F5, R2) @?= [True]
  , testCase "先手桂馬敵陣内" $ promotions (knight False Black) (F5, R3) @?= [False, True]

  , testCase "後手桂馬敵陣内" $ promotions (knight False White) (F5, R7) @?= [False, True]
  , testCase "後手桂馬敵陣内" $ promotions (knight False White) (F5, R8) @?= [True]
  , testCase "後手桂馬敵陣内" $ promotions (knight False White) (F5, R9) @?= [True]
  ]

newtype TColor = TColor Color deriving (Show)
newtype TFile  = TFile  File  deriving (Show)
newtype TRank  = TRank  Rank  deriving (Show)
newtype TBlackPromotionRank = TBlackPromotionRank Rank deriving (Show)
newtype TBlackRank          = TBlackRank          Rank deriving (Show)
newtype TWhitePromotionRank = TWhitePromotionRank Rank deriving (Show)
newtype TWhiteRank          = TWhiteRank          Rank deriving (Show)

instance (Monad m) => Serial m TColor where
  series = series' TColor [minBound .. maxBound]

instance (Monad m) => Serial m TFile where
  series = series' TFile [minBound .. maxBound]

instance (Monad m) => Serial m TRank where
  series = series' TRank [minBound .. maxBound]

instance (Monad m) => Serial m TBlackPromotionRank where
  series = series' TBlackPromotionRank [minBound .. R3]

instance (Monad m) => Serial m TBlackRank where
  series = series' TBlackRank [R4 .. maxBound]

instance (Monad m) => Serial m TWhitePromotionRank where
  series = series' TWhitePromotionRank [R7 .. maxBound]

instance (Monad m) => Serial m TWhiteRank where
  series = series' TWhiteRank [minBound .. R6]

series' :: (Enum a, Bounded a) => (a -> b) -> [a] -> Series m b
series' a bs = generate $ \depth -> map a (take depth bs)

smallChecks :: TestTree
smallChecks = testGroup "Small Checks"
  [ SC.testProperty "先手銀将敵陣内" $ \(TFile file) (TBlackPromotionRank rank) -> promotions (silver False Black) (file, rank) == [False, True]
  , SC.testProperty "後手銀将敵陣内" $ \(TFile file) (TWhitePromotionRank rank) -> promotions (silver False White) (file, rank) == [False, True]

  , SC.testProperty "先手角行敵陣内" $ \(TFile file) (TBlackPromotionRank rank) -> promotions (bishop False Black) (file, rank) == [False, True]
  , SC.testProperty "後手角行敵陣内" $ \(TFile file) (TWhitePromotionRank rank) -> promotions (bishop False White) (file, rank) == [False, True]

  , SC.testProperty "先手飛車敵陣内" $ \(TFile file) (TBlackPromotionRank rank) -> promotions (rook False Black) (file, rank) == [False, True]
  , SC.testProperty "後手飛車敵陣内" $ \(TFile file) (TWhitePromotionRank rank) -> promotions (rook False White) (file, rank) == [False, True]

  , SC.testProperty "先手歩兵" $ \(TFile file) (TBlackRank rank) -> promotions (pawn   False Black) (file, rank) == [False]
  , SC.testProperty "後手歩兵" $ \(TFile file) (TWhiteRank rank) -> promotions (pawn   False White) (file, rank) == [False]
  , SC.testProperty "先手香車" $ \(TFile file) (TBlackRank rank) -> promotions (lance  False Black) (file, rank) == [False]
  , SC.testProperty "後手香車" $ \(TFile file) (TWhiteRank rank) -> promotions (lance  False White) (file, rank) == [False]
  , SC.testProperty "先手桂馬" $ \(TFile file) (TBlackRank rank) -> promotions (knight False Black) (file, rank) == [False]
  , SC.testProperty "後手桂馬" $ \(TFile file) (TWhiteRank rank) -> promotions (knight False White) (file, rank) == [False]
  , SC.testProperty "先手銀将" $ \(TFile file) (TBlackRank rank) -> promotions (silver False Black) (file, rank) == [False]
  , SC.testProperty "後手銀将" $ \(TFile file) (TWhiteRank rank) -> promotions (silver False White) (file, rank) == [False]
  , SC.testProperty "先手角行" $ \(TFile file) (TBlackRank rank) -> promotions (bishop False Black) (file, rank) == [False]
  , SC.testProperty "後手角行" $ \(TFile file) (TWhiteRank rank) -> promotions (bishop False White) (file, rank) == [False]
  , SC.testProperty "先手飛車" $ \(TFile file) (TBlackRank rank) -> promotions (rook   False Black) (file, rank) == [False]
  , SC.testProperty "後手飛車" $ \(TFile file) (TWhiteRank rank) -> promotions (rook   False White) (file, rank) == [False]

  , SC.testProperty "金将" $ \(TColor color) (TFile file) (TRank rank) -> promotions (gold        color) (file, rank) == [False]
  , SC.testProperty "王将" $ \(TColor color) (TFile file) (TRank rank) -> promotions (gold        color) (file, rank) == [False]
  , SC.testProperty "と金" $ \(TColor color) (TFile file) (TRank rank) -> promotions (pawn   True color) (file, rank) == [False]
  , SC.testProperty "成香" $ \(TColor color) (TFile file) (TRank rank) -> promotions (lance  True color) (file, rank) == [False]
  , SC.testProperty "成桂" $ \(TColor color) (TFile file) (TRank rank) -> promotions (knight True color) (file, rank) == [False]
  , SC.testProperty "成銀" $ \(TColor color) (TFile file) (TRank rank) -> promotions (silver True color) (file, rank) == [False]
  , SC.testProperty "龍馬" $ \(TColor color) (TFile file) (TRank rank) -> promotions (bishop True color) (file, rank) == [False]
  , SC.testProperty "龍王" $ \(TColor color) (TFile file) (TRank rank) -> promotions (rook   True color) (file, rank) == [False]
  ]
