module ShogiTest.HirateTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Time.Clock
import Shogi
import Shogi.Clock (clock)
import GameClock.Clock (suddenDeath)

tests :: TestTree
tests = testGroup "hirate"
  [ testCaseSteps "Game" ゲーム
  ]

ゲーム :: (String -> IO ()) -> Assertion
ゲーム step = do
  step "平手スタート"
  time  <- getCurrentTime
  let shogi = hirate (clock $ suddenDeath 10 10) time
  putStrLn (show shogi)
  step "終了"
