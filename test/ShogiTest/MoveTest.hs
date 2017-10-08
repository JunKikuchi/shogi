module ShogiTest.MoveTest (tests) where

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
tests = testGroup "move"
  [ testCaseSteps "駒を動かす" 駒を動かす
  ]

駒を動かす :: (String -> IO ()) -> IO ()
駒を動かす step = do
  step "平手開始"
  time <- getCurrentTime
  let shogi = Shogi.hirate (clock $ GameClock.Clock.suddenDeath 1 (60 * 10)) time

  shogi1 <- 先手26歩 step shogi
  shogi2 <- 後手84歩 step shogi1

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
