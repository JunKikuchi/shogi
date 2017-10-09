module ShogiTest.MoveTest.MovePieceTest (tests) where

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
import qualified GameClock.Clock (suddenDeath, countdown)

tests :: TestTree
tests = testGroup "MovePiece"
  [ testCaseSteps "駒を動かす"    駒を動かす
  , testCaseSteps "先手詰み"      先手詰み
  , testCaseSteps "後手詰み"      後手詰み
  , testCaseSteps "先手時間切れ"  先手時間切れ
  , testCaseSteps "後手時間切れ"  後手時間切れ
  ]

駒を動かす :: (String -> IO ()) -> IO ()
駒を動かす step = do
  step "平手開始"
  time <- getCurrentTime
  let shogi = Shogi.hirate (clock $ GameClock.Clock.suddenDeath 1 (60 * 10)) time

  shogi1 <- 先手26歩 step shogi
  shogi2 <- 後手84歩 step shogi1

  step "手順"
  let moves' = shogiMoves shogi2
  length moves' @?= 2

  step "手順1"
  let move1 = moves' !! 1
  moveColor    move1 @?= Black
  moveMoveType move1 @?= movePiece (F2, R7) ((F2, R6), False)
  moveSec      move1 @?= 1
  moveTime     move1 @?= addUTCTime (fromInteger 1) time

  step "手順2"
  let move2 = moves' !! 0
  moveColor    move2 @?= White
  moveMoveType move2 @?= movePiece (F8, R3) ((F8, R4), False)
  moveSec      move2 @?= 3
  moveTime     move2 @?= addUTCTime (fromInteger 3) (statTime . currentStat $ shogi1)

  先手27歩には指せない step shogi2
  後手83歩には指せない step shogi2

  return ()

先手26歩 :: (String -> IO ()) -> Shogi -> IO Shogi
先手26歩 step shogi = do
  step "先手26歩"

  let stat   = currentStat shogi
  let time   = addUTCTime (fromInteger 1) $ statTime stat
  let clock' = statClock stat
  let shogi' = fromJust $ Shogi.move (movePiece (F2, R7) ((F2, R6), False)) 1 time shogi

  step "手番"
  statColor (currentStat shogi') @?= White

  step "将棋盤"
  statPosition (currentStat shogi') @?= Shogi.Position.fromLists ([ ((F9, R1), Piece.lance  False White)
                                                                  , ((F8, R1), Piece.knight False White)
                                                                  , ((F7, R1), Piece.silver False White)
                                                                  , ((F6, R1), Piece.gold         White)
                                                                  , ((F5, R1), Piece.king         White)
                                                                  , ((F4, R1), Piece.gold         White)
                                                                  , ((F3, R1), Piece.silver False White)
                                                                  , ((F2, R1), Piece.knight False White)
                                                                  , ((F1, R1), Piece.lance  False White)
                                                                  , ((F8, R2), Piece.rook   False White)
                                                                  , ((F2, R2), Piece.bishop False White)
                                                                  , ((F9, R3), Piece.pawn   False White)
                                                                  , ((F8, R3), Piece.pawn   False White)
                                                                  , ((F7, R3), Piece.pawn   False White)
                                                                  , ((F6, R3), Piece.pawn   False White)
                                                                  , ((F5, R3), Piece.pawn   False White)
                                                                  , ((F4, R3), Piece.pawn   False White)
                                                                  , ((F3, R3), Piece.pawn   False White)
                                                                  , ((F2, R3), Piece.pawn   False White)
                                                                  , ((F1, R3), Piece.pawn   False White)
                                                                  , ((F9, R7), Piece.pawn   False Black)
                                                                  , ((F8, R7), Piece.pawn   False Black)
                                                                  , ((F7, R7), Piece.pawn   False Black)
                                                                  , ((F6, R7), Piece.pawn   False Black)
                                                                  , ((F5, R7), Piece.pawn   False Black)
                                                                  , ((F4, R7), Piece.pawn   False Black)
                                                                  , ((F3, R7), Piece.pawn   False Black)
                                                                  , ((F2, R6), Piece.pawn   False Black)
                                                                  , ((F1, R7), Piece.pawn   False Black)
                                                                  , ((F8, R8), Piece.bishop False Black)
                                                                  , ((F2, R8), Piece.rook   False Black)
                                                                  , ((F9, R9), Piece.lance  False Black)
                                                                  , ((F8, R9), Piece.knight False Black)
                                                                  , ((F7, R9), Piece.silver False Black)
                                                                  , ((F6, R9), Piece.gold         Black)
                                                                  , ((F5, R9), Piece.king         Black)
                                                                  , ((F4, R9), Piece.gold         Black)
                                                                  , ((F3, R9), Piece.silver False Black)
                                                                  , ((F2, R9), Piece.knight False Black)
                                                                  , ((F1, R9), Piece.lance  False Black)
                                                                  ], [])

  step "手順"
  let move' = head . shogiMoves $ shogi'
  moveColor    move' @?= Black
  moveMoveType move' @?= movePiece (F2, R7) ((F2, R6), False)
  moveSec      move' @?= 1
  moveTime     move' @?= time

  let clock'' = statClock $ currentStat shogi'

  step "先手の時計"
  GameClock.lookup Black clock'' @?= GameClock.Clock.countdown 1 (GameClock.lookup Black clock')

  step "後手の時計"
  GameClock.lookup White clock'' @?= GameClock.lookup White clock'

  step "結果"
  shogiResult shogi' @?= InProgress

  return shogi'

後手84歩 :: (String -> IO ()) -> Shogi -> IO Shogi
後手84歩 step shogi = do
  step "後手84歩"

  let stat   = currentStat shogi
  let time   = addUTCTime (fromInteger 3) $ statTime stat
  let clock' = statClock stat
  let shogi' = fromJust $ Shogi.move (movePiece (F8, R3) ((F8, R4), False)) 3 time shogi

  step "手番"
  statColor (currentStat shogi') @?= Black

  step "将棋盤"
  statPosition (currentStat shogi') @?= Shogi.Position.fromLists ([ ((F9, R1), Piece.lance  False White)
                                                                  , ((F8, R1), Piece.knight False White)
                                                                  , ((F7, R1), Piece.silver False White)
                                                                  , ((F6, R1), Piece.gold         White)
                                                                  , ((F5, R1), Piece.king         White)
                                                                  , ((F4, R1), Piece.gold         White)
                                                                  , ((F3, R1), Piece.silver False White)
                                                                  , ((F2, R1), Piece.knight False White)
                                                                  , ((F1, R1), Piece.lance  False White)
                                                                  , ((F8, R2), Piece.rook   False White)
                                                                  , ((F2, R2), Piece.bishop False White)
                                                                  , ((F9, R3), Piece.pawn   False White)
                                                                  , ((F8, R4), Piece.pawn   False White)
                                                                  , ((F7, R3), Piece.pawn   False White)
                                                                  , ((F6, R3), Piece.pawn   False White)
                                                                  , ((F5, R3), Piece.pawn   False White)
                                                                  , ((F4, R3), Piece.pawn   False White)
                                                                  , ((F3, R3), Piece.pawn   False White)
                                                                  , ((F2, R3), Piece.pawn   False White)
                                                                  , ((F1, R3), Piece.pawn   False White)
                                                                  , ((F9, R7), Piece.pawn   False Black)
                                                                  , ((F8, R7), Piece.pawn   False Black)
                                                                  , ((F7, R7), Piece.pawn   False Black)
                                                                  , ((F6, R7), Piece.pawn   False Black)
                                                                  , ((F5, R7), Piece.pawn   False Black)
                                                                  , ((F4, R7), Piece.pawn   False Black)
                                                                  , ((F3, R7), Piece.pawn   False Black)
                                                                  , ((F2, R6), Piece.pawn   False Black)
                                                                  , ((F1, R7), Piece.pawn   False Black)
                                                                  , ((F8, R8), Piece.bishop False Black)
                                                                  , ((F2, R8), Piece.rook   False Black)
                                                                  , ((F9, R9), Piece.lance  False Black)
                                                                  , ((F8, R9), Piece.knight False Black)
                                                                  , ((F7, R9), Piece.silver False Black)
                                                                  , ((F6, R9), Piece.gold         Black)
                                                                  , ((F5, R9), Piece.king         Black)
                                                                  , ((F4, R9), Piece.gold         Black)
                                                                  , ((F3, R9), Piece.silver False Black)
                                                                  , ((F2, R9), Piece.knight False Black)
                                                                  , ((F1, R9), Piece.lance  False Black)
                                                                  ], [])

  step "手順"
  let move' = head . shogiMoves $ shogi'
  moveColor    move' @?= White
  moveMoveType move' @?= movePiece (F8, R3) ((F8, R4), False)
  moveSec      move' @?= 3
  moveTime     move' @?= time

  let clock'' = statClock $ currentStat shogi'

  step "先手の時計"
  GameClock.lookup Black clock'' @?= GameClock.lookup Black clock'

  step "後手の時計"
  GameClock.lookup White clock'' @?= GameClock.Clock.countdown 3 (GameClock.lookup White clock')

  step "結果"
  shogiResult shogi' @?= InProgress

  return shogi'

先手27歩には指せない :: (String -> IO ()) -> Shogi -> IO ()
先手27歩には指せない step shogi = do
  step "先手27歩には指せない"

  let stat   = currentStat shogi
  let time   = addUTCTime (fromInteger 1) $ statTime stat
  let shogi' = Shogi.move (movePiece (F2, R6) ((F2, R7), False)) 1 time shogi

  step "指せない"
  shogi' @?= Nothing

後手83歩には指せない :: (String -> IO ()) -> Shogi -> IO ()
後手83歩には指せない step shogi = do
  step "後手83歩には指せない"

  let stat   = currentStat shogi
  let time   = addUTCTime (fromInteger 1) $ statTime stat
  let shogi' = Shogi.move (movePiece (F8, R4) ((F8, R3), False)) 1 time shogi

  step "指せない"
  shogi' @?= Nothing


{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V歩V金          R7
                            R8
             王             R9
--}
先手詰み :: (String -> IO ()) -> IO ()
先手詰み step = do
  time1 <- getCurrentTime
  let position1 = Shogi.Position.fromLists ([((F5, R7), pawn False White), ((F4, R7), gold White), ((F5, R9), king Black)], [])
  let clock1    = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1    = Shogi.initShogi White position1 clock1 time1

  step "後手58金"
  let time2  = addUTCTime (fromInteger 1) time1
  let shogi2 = fromJust $ Shogi.move (movePiece (F4, R7) ((F5, R8), False)) 1 time2 shogi1

  step "詰み"
  shogiResult shogi2 @?= Win White Checkmate

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 2

  step "局面1"
  let stat1  = stats !! 1
  statColor    stat1 @?= White
  statPosition stat1 @?= Shogi.Position.fromLists ([((F5, R7), pawn False White), ((F4, R7), gold White), ((F5, R9), king Black)], [])
  statClock    stat1 @?= clock1
  statTime     stat1 @?= time1

  step "局面2"
  let stat2 = stats !! 0
  statColor    stat2 @?= Black
  statPosition stat2 @?= Shogi.Position.fromLists ([((F5, R7), pawn False White), ((F5, R8), gold White), ((F5, R9), king Black)], [])
  statClock    stat2 @?= GameClock.countdown 1 White clock1
  statTime     stat2 @?= time2

  step "手順"
  let moves' = shogiMoves shogi2
  let move1  = head moves'
  length moves' @?= 1
  moveColor    move1 @?= White
  moveMoveType move1 @?= movePiece (F4, R7) ((F5, R8), False)
  moveSec      move1 @?= 1
  moveTime     move1 @?= time2

  return ()

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
             歩 金          R3
--}
後手詰み :: (String -> IO ()) -> IO ()
後手詰み step = do
  time1 <- getCurrentTime
  let position1 = Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R3), pawn False Black), ((F4, R3), gold Black)], [])
  let clock1    = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1    = Shogi.initShogi Black position1 clock1 time1

  step "先手52金"
  let time2  = addUTCTime (fromInteger 1) time1
  let shogi2 = fromJust $ Shogi.move (movePiece (F4, R3) ((F5, R2), False)) 1 time2 shogi1

  step "詰み"
  shogiResult shogi2 @?= Win Black Checkmate

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 2

  step "局面1"
  let stat1  = stats !! 1
  statColor    stat1 @?= Black
  statPosition stat1 @?= Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R3), pawn False Black), ((F4, R3), gold Black)], [])
  statClock    stat1 @?= clock1
  statTime     stat1 @?= time1

  step "局面2"
  let stat2 = stats !! 0
  statColor    stat2 @?= White
  statPosition stat2 @?= Shogi.Position.fromLists ([((F5, R1), king White), ((F5, R3), pawn False Black), ((F5, R2), gold Black)], [])
  statClock    stat2 @?= GameClock.countdown 1 Black clock1
  statTime     stat2 @?= time2

  step "手順"
  let moves' = shogiMoves shogi2
  let move'  = head moves'
  length moves' @?= 1
  moveColor    move' @?= Black
  moveMoveType move' @?= movePiece (F4, R3) ((F5, R2), False)
  moveSec      move' @?= 1
  moveTime     move' @?= time2

  return ()

先手時間切れ :: (String -> IO ()) -> IO ()
先手時間切れ step = do
  time <- getCurrentTime
  let clock' = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi  = Shogi.hirate clock' time

  step "先手26歩"
  let sec1   = 60 * 10
  let time1  = addUTCTime (fromInteger sec1) $ statTime $ currentStat shogi
  let shogi1 = fromJust $ Shogi.move (movePiece (F2, R7) ((F2, R6), False)) (fromIntegral sec1) time1 shogi

  step "時間切れ"
  shogiResult shogi1 @?= Win White TimeForfeit

  step "局面"
  let stats = shogiStats shogi1
  length stats @?= 1

  step "局面1"
  let stat  = stats !! 0
  statColor    stat @?= Black
  statPosition stat @?= Shogi.Position.hirate
  statClock    stat @?= GameClock.countdown (60 * 10) Black clock'
  statTime     stat @?= time

  step "手順"
  shogiMoves  shogi1 @?= []

  return ()

後手時間切れ :: (String -> IO ()) -> IO ()
後手時間切れ step = do
  time <- getCurrentTime
  let clock' = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi  = Shogi.hirate clock' time

  step "先手26歩"
  let sec1   = 10
  let time1  = addUTCTime (fromInteger sec1) $ statTime $ currentStat shogi
  let shogi1 = fromJust $ Shogi.move (movePiece (F2, R7) ((F2, R6), False)) (fromIntegral sec1) time1 shogi

  step "後手84歩"
  let sec2   = 60 * 10
  let time2  = addUTCTime (fromInteger sec2) $ statTime $ currentStat shogi1
  let shogi2 = fromJust $ Shogi.move (movePiece (F8, R3) ((F8, R4), False)) (fromIntegral sec2) time2 shogi1

  step "時間切れ"
  shogiResult shogi2 @?= Win Black TimeForfeit

  step "局面"
  let stats = shogiStats shogi1
  length stats @?= 2

  step "局面1"
  let stat1  = stats !! 1
  statColor    stat1 @?= Black
  statPosition stat1 @?= Shogi.Position.hirate
  statClock    stat1 @?= clock'
  statTime     stat1 @?= time

  step "局面2"
  let stat2  = stats !! 0
  statColor    stat2 @?= White
  statPosition stat2 @?= (fromJust $ Shogi.Position.move ((F2, R7), Black) ((F2, R6), False) Shogi.Position.hirate)
  statClock    stat2 @?= GameClock.countdown 10 Black clock'
  statTime     stat2 @?= time1

  step "手順"
  let moves' = shogiMoves shogi2
  let move'  = head moves'
  length moves' @?= 1
  moveColor    move' @?= Black
  moveMoveType move' @?= movePiece (F2, R7) ((F2, R6), False)
  moveSec      move' @?= 10
  moveTime     move' @?= time1

  return ()
