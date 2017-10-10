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
  let stat1  = shogiStat shogi1
  let moves1 = shogiMoves shogi1
  let move1  = head moves1

  step "最新の局面"
  statColor    stat1 @?= Black
  statPosition stat1 @?= Position.hirate
  statClock    stat1 @?= clock1
  statTime     stat1 @?= time1

  step "手順リスト"
  length moves1 @?= 1
  moveColor move1 @?= Black
  moveType  move1 @?= Init
  moveSec   move1 @?= 0
  moveTime  move1 @?= time1
  moveStat  move1 @?= stat1

  step "結果"
  shogiResult shogi1 @?= InProgress

  return ()
