module ShogiTest.CountdownTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.Time.Clock
import Shogi
import Shogi.Clock (Clock, clock)
import Shogi.Color
import Shogi.Square
import qualified GameClock as GC
import GameClock.Clock (suddenDeath)

tests :: TestTree
tests = testGroup "countdown"
  [ testCaseSteps "先手の時計を進める" 先手の時計を進める
  , testCaseSteps "後手の時計を進める" 後手の時計を進める
  , testCaseSteps "先手時間切れ"       先手時間切れ
  , testCaseSteps "後手時間切れ"       後手時間切れ
  ]

先手の時計を進める :: (String -> IO ()) -> IO ()
先手の時計を進める step = do
  step "先手番の局面を作成"
  time <- getCurrentTime
  let shogi' = countdown 10 time (newShogi time)
  assertBool "結果は Just" $ isJust shogi'

  let shogi = fromJust shogi'
  let stat  = currentStat $ shogi

  step "時計が進む"
  statClock stat @?= GC.countdown 10 Black shogiClock

  step "手番は変わらない"
  statColor stat @?= Black

  step "対局中"
  shogiResult shogi @?= InProgress

後手の時計を進める :: (String -> IO ()) -> IO ()
後手の時計を進める step = do
  step "後手番の局面を作成"
  time <- getCurrentTime
  let mv     = movePiece (F2, R7) ((F2, R7), False)
  let shogi' = move mv 5 time (newShogi time) >>= countdown 10 time
  assertBool "結果は Just" $ isJust shogi'

  let shogi = fromJust shogi'
  let stat  = currentStat $ shogi

  step "時計が進む"
  statClock stat @?= GC.countdown 10 White shogiClock

  step "手番は変わらない"
  statColor stat @?= White

  step "対局中"
  shogiResult shogi @?= InProgress

先手時間切れ :: (String -> IO ()) -> IO ()
先手時間切れ step = do
  step "先手番の局面を作成"
  time <- getCurrentTime
  let shogi' = countdown (60 * 10 + 1) time (newShogi time)
  assertBool "結果は Just" $ isJust shogi'

  let shogi = fromJust shogi'
  let stat  = currentStat $ shogi

  step "時計が進む"
  statClock stat @?= GC.countdown (60 * 10 + 1) Black shogiClock

  step "手番は変わらない"
  statColor stat @?= Black

  step "時間切れで対局終了"
  shogiResult shogi @?= Win White TimeForfeit

後手時間切れ :: (String -> IO ()) -> IO ()
後手時間切れ step = do
  step "後手番の局面を作成"
  time <- getCurrentTime
  let mv     = movePiece (F2, R7) ((F2, R7), False)
  let shogi' = move mv 5 time (newShogi time) >>= countdown (60 * 10 + 1) time
  assertBool "結果は Just" $ isJust shogi'

  let shogi = fromJust shogi'
  let stat  = currentStat $ shogi

  step "時計が進む"
  statClock stat @?= GC.countdown (60 * 10 + 1) White shogiClock

  step "手番は変わらない"
  statColor stat @?= White

  step "時間切れで対局終了"
  shogiResult shogi @?= Win Black TimeForfeit

newShogi :: UTCTime -> Shogi
newShogi = Shogi.hirate shogiClock

shogiClock :: Clock
shogiClock = clock $ suddenDeath 1 (60 * 10)
