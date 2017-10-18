module ShogiTest.HirateTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Time.Clock
import Shogi
import Shogi.Clock (clock)
import Shogi.Color
import GameClock.Clock (suddenDeath)
import qualified Shogi.Position as Position

tests :: TestTree
tests = testGroup "hirate"
  [ testCaseSteps "平手作成" 平手作成
  ]

平手作成 :: (String -> IO ()) -> IO ()
平手作成 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let shogi1 = hirate clock1 time1
  let state1 = shogiState shogi1
  let moves1 = shogiMoves shogi1
  let move1  = head moves1

  step "最新の局面"
  stateColor    state1 @?= Black
  statePosition state1 @?= Position.hirate
  stateClock    state1 @?= clock1
  stateTime     state1 @?= time1

  step "手順リスト"
  length moves1 @?= 1
  moveColor move1 @?= Black
  moveType  move1 @?= Init
  moveSec   move1 @?= 0
  moveTime  move1 @?= time1
  moveState move1 @?= state1

  step "結果"
  shogiResult shogi1 @?= InProgress

  return ()
