module ShogiTest.CountdownTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.Time.Clock
import Shogi
import Shogi.Clock
import qualified Shogi.Position as Position
import qualified GameClock
import GameClock.Clock (suddenDeath)

tests :: TestTree
tests = testGroup "countdown"
  [ testCaseSteps "先手の時計を進める" 先手の時計を進める
  , testCaseSteps "後手の時計を進める" 後手の時計を進める
  , testCaseSteps "先手時間切れ"       先手時間切れ
  , testCaseSteps "後手時間切れ"       後手時間切れ
  , testCaseSteps "先手対局中以外"     先手対局中以外
  , testCaseSteps "後手対局中以外"     後手対局中以外
  ]

先手の時計を進める :: (String -> IO ()) -> IO ()
先手の時計を進める step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let shogi1 = hirate clock1 time1
  let state1 = shogiState shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ countdown 10 time2 shogi1
  let state2 = shogiState shogi2
  let clock2 = GameClock.countdown 10 Black clock1

  step "最新の局面は時計以外は同じ"
  stateColor    state2 @?= stateColor    state1
  statePosition state2 @?= statePosition state1

  step "時計は更新される"
  stateClock state2 @?= clock2
  stateTime  state2 @?= time2

  step "手順リストは変わらず"
  shogiMoves shogi2 @?= shogiMoves shogi1

  step "結果は変わらず"
  shogiResult shogi2 @?= shogiResult shogi1

  return ()

後手の時計を進める :: (String -> IO ()) -> IO ()
後手の時計を進める step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let shogi1 = shogi White Position.hirate clock1 time1
  let state1 = shogiState shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ countdown 10 time2 shogi1
  let state2 = shogiState shogi2
  let clock2 = GameClock.countdown 10 White clock1

  step "最新の局面は時計以外は同じ"
  stateColor    state2 @?= stateColor    state1
  statePosition state2 @?= statePosition state1

  step "時計は更新される"
  stateClock state2 @?= clock2
  stateTime  state2 @?= time2

  step "手順リストは変わらず"
  shogiMoves shogi2 @?= shogiMoves shogi1

  step "結果は変わらず"
  shogiResult shogi2 @?= shogiResult shogi1

  return ()

先手時間切れ :: (String -> IO ()) -> IO ()
先手時間切れ step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let shogi1 = hirate clock1 time1
  let state1 = shogiState shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ countdown 10 time2 shogi1
  let state2 = shogiState shogi2
  let clock2 = GameClock.countdown 10 Black clock1

  step "最新の局面は時計以外は同じ"
  stateColor    state2 @?= stateColor    state1
  statePosition state2 @?= statePosition state1

  step "時計は更新される"
  stateClock state2 @?= clock2
  stateTime  state2 @?= time2

  step "手順リストに時間切れ追加"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2

  let move2 = moves2 !! 0
  moveColor move2 @?= Black
  moveType  move2 @?= TimeIsUp
  moveSec   move2 @?= 10
  moveTime  move2 @?= time2
  moveState move2 @?= state2

  (moves2 !! 1) @?= (shogiMoves shogi1 !! 0)

  step "結果は時間切れで後手の勝ち"
  shogiResult shogi2 @?= Win White TimeForfeit

  return ()

後手時間切れ :: (String -> IO ()) -> IO ()
後手時間切れ step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let shogi1 = shogi White Position.hirate clock1 time1
  let state1 = shogiState shogi1

  let time2  = addUTCTime 10 time1
  let shogi2 = fromJust $ countdown 10 time2 shogi1
  let state2 = shogiState shogi2
  let clock2 = GameClock.countdown 10 White clock1

  step "最新の局面は時計以外は同じ"
  stateColor    state2 @?= stateColor    state1
  statePosition state2 @?= statePosition state1

  step "時計は更新される"
  stateClock state2 @?= clock2
  stateTime  state2 @?= time2

  step "手順リストに時間切れ追加"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2

  let move2 = moves2 !! 0
  moveColor move2 @?= White
  moveType  move2 @?= TimeIsUp
  moveSec   move2 @?= 10
  moveTime  move2 @?= time2
  moveState move2 @?= state2

  (moves2 !! 1) @?= (shogiMoves shogi1 !! 0)

  step "結果は時間切れで先手の勝ち"
  shogiResult shogi2 @?= Win Black TimeForfeit

  return ()

先手対局中以外 :: (String -> IO ()) -> IO ()
先手対局中以外 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let shogi1 = (return $ hirate clock1 time1) >>= countdown 10 (addUTCTime 10 time1) >>= countdown 10 (addUTCTime 20 time1)

  step "終了した対局"
  shogi1 @?= Nothing

  return ()

後手対局中以外 :: (String -> IO ()) -> IO ()
後手対局中以外 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let shogi1 = (return $ shogi White Position.hirate clock1 time1) >>= countdown 10 (addUTCTime 10 time1) >>= countdown 10 (addUTCTime 20 time1)

  step "終了した対局"
  shogi1 @?= Nothing

  return ()
