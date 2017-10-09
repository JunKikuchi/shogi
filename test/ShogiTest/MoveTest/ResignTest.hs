module ShogiTest.MoveTest.ResignTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.Time.Clock
import Shogi
import Shogi.Position
import Shogi.Clock (clock)
import Shogi.Color
import Shogi.Square
import Shogi.Piece as Piece
import qualified GameClock
import qualified GameClock.Clock (suddenDeath)

tests :: TestTree
tests = testGroup "Resign"
  [ testCaseSteps "先手投了"         先手投了
  , testCaseSteps "後手投了"         後手投了
  , testCaseSteps "先手投了時間切れ" 先手投了時間切れ
  , testCaseSteps "後手投了時間切れ" 後手投了時間切れ
  ]

先手投了 :: (String -> IO ()) -> IO ()
先手投了 step = do
  time1 <- getCurrentTime
  let pos1   = Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi Black pos1 clock1 time1

  step "先手投了"
  let time2  = addUTCTime (fromInteger 1) time1
  let shogi2 = fromJust $ Shogi.move (resign) 1 time2 shogi1

  step "結果"
  shogiResult shogi2 @?= Win White Resignation

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 1

  step "局面1"
  let stat1 = stats !! 0
  statColor    stat1 @?= Black
  statPosition stat1 @?= Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  statClock    stat1 @?= clock1
  statTime     stat1 @?= time1

  step "手順"
  let moves' = shogiMoves shogi2
  length moves' @?= 1

  step "手順1"
  let move1 = moves' !! 0
  moveColor    move1 @?= Black
  moveMoveType move1 @?= resign
  moveSec      move1 @?= 1
  moveTime     move1 @?= time2

  return ()

後手投了 :: (String -> IO ()) -> IO ()
後手投了 step = do
  time1 <- getCurrentTime
  let pos1   = Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi White pos1 clock1 time1

  step "後手投了"
  let time2  = addUTCTime (fromInteger 1) time1
  let shogi2 = fromJust $ Shogi.move (resign) 1 time2 shogi1

  step "結果"
  shogiResult shogi2 @?= Win Black Resignation

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 1

  step "局面1"
  let stat1 = stats !! 0
  statColor    stat1 @?= White
  statPosition stat1 @?= Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  statClock    stat1 @?= clock1
  statTime     stat1 @?= time1

  step "手順"
  let moves' = shogiMoves shogi2
  length moves' @?= 1

  step "手順1"
  let move1 = moves' !! 0
  moveColor    move1 @?= White
  moveMoveType move1 @?= resign
  moveSec      move1 @?= 1
  moveTime     move1 @?= time2

  return ()

先手投了時間切れ :: (String -> IO ()) -> IO ()
先手投了時間切れ step = do
  time1 <- getCurrentTime
  let pos1   = Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi Black pos1 clock1 time1

  step "先手投了時間切れ"
  let time2  = addUTCTime (fromInteger (60 * 10)) time1
  let shogi2 = fromJust $ Shogi.move (resign) (60 * 10) time2 shogi1

  step "結果"
  shogiResult shogi2 @?= Win White TimeForfeit

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 1

  step "局面1"
  let stat1 = stats !! 0
  statColor    stat1 @?= Black
  statPosition stat1 @?= Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  statClock    stat1 @?= GameClock.countdown (60 * 10) Black clock1
  statTime     stat1 @?= time1

  step "手順"
  shogiMoves shogi2 @?= []

  return ()

後手投了時間切れ :: (String -> IO ()) -> IO ()
後手投了時間切れ step = do
  time1 <- getCurrentTime
  let pos1   = Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi White pos1 clock1 time1

  step "後手投了時間切れ"
  let time2  = addUTCTime (fromInteger (60 * 10)) time1
  let shogi2 = fromJust $ Shogi.move (resign) (60 * 10) time2 shogi1

  step "結果"
  shogiResult shogi2 @?= Win Black TimeForfeit

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 1

  step "局面1"
  let stat1 = stats !! 0
  statColor    stat1 @?= White
  statPosition stat1 @?= Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R9), king Black)], [])
  statClock    stat1 @?= GameClock.countdown (60 * 10) White clock1
  statTime     stat1 @?= time1

  step "手順"
  shogiMoves shogi2 @?= []

  return ()
