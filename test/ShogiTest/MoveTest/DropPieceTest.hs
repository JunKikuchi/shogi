module ShogiTest.MoveTest.DropPieceTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.Time.Clock
import Shogi
import Shogi.Clock
import Shogi.Color
import Shogi.Square
import Shogi.Piece
import qualified Shogi.Position as Position
import qualified GameClock
import GameClock.Clock (suddenDeath)

tests :: TestTree
tests = testGroup "DropPiece"
  [ testCaseSteps "持ち駒を打つ"   持ち駒を打つ
  , testCaseSteps "先手二歩"       先手二歩
  , testCaseSteps "後手二歩"       後手二歩
  , testCaseSteps "先手詰み"       先手詰み
  , testCaseSteps "後手詰み"       後手詰み
  , testCaseSteps "先手詰み回避"   先手詰み回避
  , testCaseSteps "後手詰み回避"   後手詰み回避
  , testCaseSteps "先手打ち歩詰め" 先手打ち歩詰め
  , testCaseSteps "後手打ち歩詰め" 後手打ち歩詰め
  , testCaseSteps "先手時間切れ"   先手時間切れ
  , testCaseSteps "後手時間切れ"   後手時間切れ
  ]

{--
 V金V歩
 F9 F8 F7 F6 F5 F4 F3 F2 F1
             V王            R1
                            R2
              歩            R3
                            R4
                            R5
                            R6
             V歩            R7
                            R8
              王            R9
 金 歩
--}
持ち駒を打つ :: (String -> IO ()) -> IO ()
持ち駒を打つ step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F5, R9), king Black)
                                   ]
                                  ,[ gold White
                                   , pawn False White
                                   , gold Black
                                   , pawn False Black
                                   ])
  let shogi1 = shogi Black pos1 clock1 time1

  step "[先手43歩]"
  let time2  = addUTCTime 1 time1
  let move2  = dropPiece (pawn False Black) (F4, R3)
  let shogi2 = fromJust $ move move2 1 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 1 Black clock1
  statColor    stat2 @?= White
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False Black)
                                             , ((F4, R3), pawn False Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F5, R9), king Black)
                                             ]
                                            ,[ gold White
                                             , pawn False White
                                             , gold Black
                                             ])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move Black move2 1 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "対局中"
  shogiResult shogi2 @?= InProgress

  step "[後手47歩]"
  let time3  = addUTCTime 1 time2
  let move3  = dropPiece (pawn False White) (F4, R7)
  let shogi3 = fromJust $ move move3 1 time3 shogi2

  step "局面"
  let stat3  = shogiStat shogi3
  let clock3 = GameClock.countdown 1 Black clock2
  statColor    stat3 @?= White
  statPosition stat3 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False Black)
                                             , ((F4, R3), pawn False Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F4, R7), pawn False White)
                                             , ((F5, R9), king Black)
                                             ]
                                            ,[ gold White
                                             , gold Black
                                             ])
  statClock    stat3 @?= clock3
  statTime     stat3 @?= time3

  step "手順"
  let moves3 = shogiMoves shogi3
  length moves3 @?= 3
  moves2 !! 0 @?= Move Black move3 1 time3 stat3
  moves2 !! 1 @?= shogiMoves shogi2 !! 0
  moves2 !! 2 @?= shogiMoves shogi2 !! 1

  step "対局中"
  shogiResult shogi2 @?= InProgress

  return ()

先手二歩 :: (String -> IO ()) -> IO ()
先手二歩 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F5, R9), king Black)
                                   ]
                                  ,[ gold White
                                   , pawn False White
                                   , gold Black
                                   , pawn False Black
                                   ])
  let shogi1 = shogi Black pos1 clock1 time1

  step "[先手54歩]"
  let time2  = addUTCTime 1 time1
  let move2  = dropPiece (pawn False Black) (F5, R4)
  move move2 1 time2 shogi1 @?= Nothing

  return ()

後手二歩 :: (String -> IO ()) -> IO ()
後手二歩 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F5, R9), king Black)
                                   ]
                                  ,[ gold White
                                   , pawn False White
                                   , gold Black
                                   , pawn False Black
                                   ])
  let shogi1 = shogi White pos1 clock1 time1

  step "[後手56歩]"
  let time2  = addUTCTime 1 time1
  let move2  = dropPiece (pawn False White) (F5, R6)
  move move2 1 time2 shogi1 @?= Nothing

  return ()

先手詰み :: (String -> IO ()) -> IO ()
先手詰み step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F5, R9), king Black)
                                   ]
                                  ,[ gold White
                                   , pawn False White
                                   , gold Black
                                   , pawn False Black
                                   ])
  let shogi1 = shogi White pos1 clock1 time1

  step "[後手58金]"
  let time2  = addUTCTime 1 time1
  let move2  = dropPiece (gold White) (F5, R8)
  let shogi2 = fromJust $ move move2 1 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 1 White clock1
  statColor    stat2 @?= White
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F5, R8), gold White)
                                             , ((F5, R9), king Black)
                                             ]
                                            ,[ pawn False White
                                             , gold Black
                                             , pawn False Black
                                             ])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move White move2 1 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "後手の勝ち"
  shogiResult shogi2 @?= Win White Checkmate

  return ()

後手詰み :: (String -> IO ()) -> IO ()
後手詰み step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F5, R9), king Black)
                                   ]
                                  ,[ gold White
                                   , pawn False White
                                   , gold Black
                                   , pawn False Black
                                   ])
  let shogi1 = shogi Black pos1 clock1 time1

  step "[先手52金]"
  let time2  = addUTCTime 1 time1
  let move2  = dropPiece (gold Black) (F5, R2)
  let shogi2 = fromJust $ move move2 1 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 1 Black clock1
  statColor    stat2 @?= Black
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R2), gold Black)
                                             , ((F5, R3), pawn False Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F5, R9), king Black)
                                             ]
                                            ,[ gold White
                                             , pawn False White
                                             , pawn False Black
                                             ])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move White move2 1 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "後手の勝ち"
  shogiResult shogi2 @?= Win Black Checkmate
  return ()

先手詰み回避 :: (String -> IO ()) -> IO ()
先手詰み回避 step = do
  step ""
  assertBool "未実装" False
  return ()

後手詰み回避 :: (String -> IO ()) -> IO ()
後手詰み回避 step = do
  step ""
  assertBool "未実装" False
  return ()

先手打ち歩詰め :: (String -> IO ()) -> IO ()
先手打ち歩詰め step = do
  step ""
  assertBool "未実装" False
  return ()

後手打ち歩詰め :: (String -> IO ()) -> IO ()
後手打ち歩詰め step = do
  step ""
  assertBool "未実装" False
  return ()

先手時間切れ :: (String -> IO ()) -> IO ()
先手時間切れ step = do
  step ""
  assertBool "未実装" False
  return ()

後手時間切れ :: (String -> IO ()) -> IO ()
後手時間切れ step = do
  step ""
  assertBool "未実装" False
  return ()
