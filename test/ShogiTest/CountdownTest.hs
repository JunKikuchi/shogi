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
  ]

先手の時計を進める :: (String -> IO ()) -> IO ()
先手の時計を進める step = do
  step "先手番の局面を作成"
  time <- getCurrentTime
  let shogi  = newShogi time
  let shogi' = countdown 10 (addSec 10 time) shogi
  assertBool "結果は Just" $ isJust shogi'

  let stat = head . shogiStats $ fromJust shogi'

  step "時計が進む"
  statClock stat @?= GC.countdown 10 Black shogiClock

  step "手番は変わらない"
  statColor stat @?= Black

後手の時計を進める :: (String -> IO ()) -> IO ()
後手の時計を進める step = do
  step "後手番の局面を作成"
  time <- getCurrentTime
  let mv     = movePiece (F2, R7) ((F2, R7), False)
  let s5     = addSec 5 time
  let shogi' = move mv 5 s5 (newShogi time) >>= countdown 10 (addSec 10 time)
  assertBool "結果は Just" $ isJust shogi'

  let stat = head . shogiStats $ fromJust shogi'

  step "時計が進む"
  statClock stat @?= GC.countdown 10 White shogiClock

  step "手番は変わらない"
  statColor stat @?= White

newShogi :: UTCTime -> Shogi
newShogi = Shogi.hirate shogiClock

shogiClock :: Clock
shogiClock = clock $ suddenDeath 1 (60 * 10)

addSec :: Integer -> UTCTime -> UTCTime
addSec = addUTCTime . fromInteger
