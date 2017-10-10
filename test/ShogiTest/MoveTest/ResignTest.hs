module ShogiTest.MoveTest.ResignTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.Time.Clock
import Shogi
import Shogi.Clock
import Shogi.Color
import qualified Shogi.Position as Position
import qualified GameClock
import GameClock.Clock (suddenDeath)

tests :: TestTree
tests = testGroup "Resign"
  [ testCaseSteps "先手投了"         先手投了
  , testCaseSteps "後手投了"         後手投了
  , testCaseSteps "先手投了時間切れ" 先手投了時間切れ
  , testCaseSteps "後手投了時間切れ" 後手投了時間切れ
  , testCaseSteps "先手対局中以外"   先手対局中以外
  , testCaseSteps "後手対局中以外"   後手対局中以外
  ]

先手投了 :: (String -> IO ()) -> IO ()
先手投了 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let shogi1 = hirate clock1 time1
  let stat1  = shogiStat shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ move resign 10 time2 shogi1
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 10 Black clock1

  step "最新の局面は時計以外は同じ"
  statColor    stat2 @?= statColor    stat1
  statPosition stat2 @?= statPosition stat1

  step "時計は更新される"
  statClock stat2 @?= clock2
  statTime  stat2 @?= time2

  step "手順リストに投了追加"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2

  let move2 = moves2 !! 0
  moveColor move2 @?= Black
  moveType  move2 @?= Resign
  moveSec   move2 @?= 10
  moveTime  move2 @?= time2
  moveStat  move2 @?= stat2

  (moves2 !! 1) @?= (shogiMoves shogi1 !! 0)

  step "結果は投了で後手の勝ち"
  shogiResult shogi2 @?= Win White Resignation

  return ()

後手投了 :: (String -> IO ()) -> IO ()
後手投了 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let shogi1 = shogi White Position.hirate clock1 time1
  let stat1  = shogiStat shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ move resign 10 time2 shogi1
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 10 White clock1

  step "最新の局面は時計以外は同じ"
  statColor    stat2 @?= statColor    stat1
  statPosition stat2 @?= statPosition stat1

  step "時計は更新される"
  statClock stat2 @?= clock2
  statTime  stat2 @?= time2

  step "手順リストに時間切れ追加"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2

  let move2 = moves2 !! 0
  moveColor move2 @?= White
  moveType  move2 @?= Resign
  moveSec   move2 @?= 10
  moveTime  move2 @?= time2
  moveStat  move2 @?= stat2

  (moves2 !! 1) @?= (shogiMoves shogi1 !! 0)

  step "結果は投了で先手の勝ち"
  shogiResult shogi2 @?= Win Black Resignation

  return ()

先手投了時間切れ :: (String -> IO ()) -> IO ()
先手投了時間切れ step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let shogi1 = hirate clock1 time1
  let stat1  = shogiStat shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ move resign 10 time2 shogi1
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 10 Black clock1

  step "最新の局面は時計以外は同じ"
  statColor    stat2 @?= statColor    stat1
  statPosition stat2 @?= statPosition stat1

  step "時計は更新される"
  statClock stat2 @?= clock2
  statTime  stat2 @?= time2

  step "手順リストに時間切れ追加"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2

  let move2 = moves2 !! 0
  moveColor move2 @?= Black
  moveType  move2 @?= TimeIsUp
  moveSec   move2 @?= 10
  moveTime  move2 @?= time2
  moveStat  move2 @?= stat2

  (moves2 !! 1) @?= (shogiMoves shogi1 !! 0)

  step "結果は時間切れで後手の勝ち"
  shogiResult shogi2 @?= Win White TimeForfeit

  return ()

後手投了時間切れ :: (String -> IO ()) -> IO ()
後手投了時間切れ step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let shogi1 = shogi White Position.hirate clock1 time1
  let stat1  = shogiStat shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ move resign 10 time2 shogi1
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 10 White clock1

  step "最新の局面は時計以外は同じ"
  statColor    stat2 @?= statColor    stat1
  statPosition stat2 @?= statPosition stat1

  step "時計は更新される"
  statClock stat2 @?= clock2
  statTime  stat2 @?= time2

  step "手順リストに時間切れ追加"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2

  let move2 = moves2 !! 0
  moveColor move2 @?= White
  moveType  move2 @?= TimeIsUp
  moveSec   move2 @?= 10
  moveTime  move2 @?= time2
  moveStat  move2 @?= stat2

  (moves2 !! 1) @?= (shogiMoves shogi1 !! 0)

  step "結果は投了で先手の勝ち"
  shogiResult shogi2 @?= Win Black TimeForfeit

  return ()

先手対局中以外 :: (String -> IO ()) -> IO ()
先手対局中以外 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let shogi1 = (return $ hirate clock1 time1) >>= countdown 10 (addUTCTime 10 time1) >>= move resign 10 (addUTCTime 20 time1)

  step "終了した対局"
  shogi1 @?= Nothing

  return ()

後手対局中以外 :: (String -> IO ()) -> IO ()
後手対局中以外 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let shogi1 = (return $ shogi White Position.hirate clock1 time1) >>= countdown 10 (addUTCTime 10 time1) >>= move resign 10 (addUTCTime 20 time1)

  step "終了した対局"
  shogi1 @?= Nothing

  return ()
