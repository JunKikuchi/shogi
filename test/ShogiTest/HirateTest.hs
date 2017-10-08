module ShogiTest.HirateTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Time.Clock
import Shogi
import Shogi.Clock (clock)
import Shogi.Color
import qualified Shogi.Position as Position
import GameClock.Clock (suddenDeath)

tests :: TestTree
tests = testGroup "hirate"
  [ testCaseSteps "平手作成" 平手作成
  ]

平手作成 :: (String -> IO ()) -> IO ()
平手作成 step = do
  time <- getCurrentTime
  let shogi  = hirate (clock $ suddenDeath 1 (60 * 10)) time
  let stats  = shogiStats shogi
  let moves  = shogiMoves shogi
  let result = shogiResult shogi
  let stat   = currentStat shogi

  step "手番の数"
  length stats @?= 1

  step "手順の数"
  length moves @?= 0

  step "結果"
  result @?= InProgress

  step "手番"
  statColor stat @?= Black

  step "局面"
  statPosition stat @?= Position.hirate

  step "時計"
  statClock stat @?= (clock $ suddenDeath 1 (60 * 10))

  step "時間"
  statTime stat @?= time
