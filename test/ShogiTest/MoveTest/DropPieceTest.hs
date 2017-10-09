module ShogiTest.MoveTest.DropPieceTest (tests) where

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
import qualified GameClock.Clock (suddenDeath {--, countdown --})

tests :: TestTree
tests = testGroup "DropPiece"
  [ testCaseSteps "持ち駒を打つ" 持ち駒を打つ
  , testCaseSteps "先手詰み"     先手詰み
  , testCaseSteps "後手詰み"     後手詰み
  , testCaseSteps "先手時間切れ" 先手時間切れ
  , testCaseSteps "後手時間切れ" 後手時間切れ
  ]

{--
 V歩 V歩
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
                            R3
                            R4
                            R5
                            R6
                            R7
                            R8
             王             R9
 歩 歩
--}
持ち駒を打つ :: (String -> IO ()) -> IO ()
持ち駒を打つ step = do
  step "将棋作成"
  time1 <- getCurrentTime
  let position1 = Shogi.Position.fromLists ([ ((F5, R1), king White)
                                            , ((F5, R9), king Black)
                                            ],
                                            [ pawn False White
                                            , pawn False White
                                            , pawn False Black
                                            , pawn False Black
                                            ])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi Black position1 clock1 time1

  step "先手53歩"
  let time2  = addUTCTime (fromInteger 1) time1
  let shogi2 = fromJust $ Shogi.move (dropPiece (pawn False Black) (F5, R3)) 1 time2 shogi1

  step "先手は手番違いで指せない"
  Shogi.move (dropPiece (pawn False Black) (F5, R9)) 1 time2 shogi2 @?= Nothing

  step "後手59歩指せない"
  Shogi.move (dropPiece (pawn False White) (F5, R9)) 1 time2 shogi2 @?= Nothing

  step "後手57歩"
  let time3  = addUTCTime (fromInteger 3) time2
  let shogi3 = fromJust $ Shogi.move (dropPiece (pawn False White) (F5, R7)) 3 time3 shogi2

  step "後手は手番違いで指せない"
  Shogi.move (dropPiece (pawn False White) (F5, R9)) 1 time3 shogi3 @?= Nothing

  step "先手51歩指せない"
  Shogi.move (dropPiece (pawn False Black) (F5, R1)) 1 time3 shogi3 @?= Nothing

  step "結果"
  shogiResult shogi3 @?= InProgress

  step "局面"
  let stats = shogiStats shogi3
  length stats @?= 3

  step "局面1"
  let stat1 = stats !! 2
  statColor    stat1 @?= Black
  statPosition stat1 @?= Shogi.Position.fromLists ([ ((F5, R1), king White)
                                                   , ((F5, R9), king Black)
                                                   ],
                                                   [ pawn False White
                                                   , pawn False White
                                                   , pawn False Black
                                                   , pawn False Black
                                                   ])
  statClock stat1 @?= clock1
  statTime  stat1 @?= time1

  step "局面2"
  let stat2  = stats !! 1
  let clock2 = GameClock.countdown 1 Black clock1
  statColor    stat2 @?= White
  statPosition stat2 @?= Shogi.Position.fromLists ([ ((F5, R1), king White)
                                                   , ((F5, R9), king Black)
                                                   , ((F5, R3), pawn False Black)
                                                   ],
                                                   [ pawn False White
                                                   , pawn False White
                                                   , pawn False Black
                                                   ])
  statClock stat2 @?= clock2
  statTime  stat2 @?= time2

  step "局面3"
  let stat3  = stats !! 0
  let clock3 = GameClock.countdown 3 White clock2
  statColor    stat3 @?= Black
  statPosition stat3 @?= Shogi.Position.fromLists ([ ((F5, R1), king White)
                                                   , ((F5, R9), king Black)
                                                   , ((F5, R3), pawn False Black)
                                                   , ((F5, R7), pawn False White)
                                                   ],
                                                   [ pawn False White
                                                   , pawn False Black
                                                   ])
  statClock stat3 @?= clock3
  statTime  stat3 @?= time3

  step "手順"
  let moves' = shogiMoves shogi3
  length moves' @?= 2

  step "手順1"
  let move1 = moves' !! 1
  moveColor    move1 @?= Black
  moveMoveType move1 @?= dropPiece (pawn False Black) (F5, R3)
  moveSec      move1 @?= 1
  moveTime     move1 @?= time2

  step "手順2"
  let move2 = moves' !! 0
  moveColor    move2 @?= White
  moveMoveType move2 @?= dropPiece (pawn False White) (F5, R7)
  moveSec      move2 @?= 3
  moveTime     move2 @?= time3

  return ()

{--
 V金
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V歩             R7
                            R8
             王             R9
--}
先手詰み :: (String -> IO ()) -> IO ()
先手詰み step = do
  time1 <- getCurrentTime
  let position1 = Shogi.Position.fromLists ([ ((F5, R7), pawn False White)
                                            , ((F5, R9), king Black)
                                            ],
                                            [ Piece.gold White
                                            ])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi White position1 clock1 time1

  step "後手58金"
  let time2  = addUTCTime (fromInteger 1) time1
  let shogi2 = fromJust $ Shogi.move (dropPiece (gold White) (F5, R8)) 1 time2 shogi1

  step "結果"
  shogiResult shogi2 @?= Win White Checkmate

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 2

  step "局面1"
  let stat1  = stats !! 1
  statColor    stat1 @?= White
  statPosition stat1 @?= Shogi.Position.fromLists ([ ((F5, R7), pawn False White)
                                                   , ((F5, R9), king Black)
                                                   ],
                                                   [ gold White
                                                   ])
  statClock    stat1 @?= clock1
  statTime     stat1 @?= time1

  step "局面2"
  let stat2 = stats !! 0
  statColor    stat2 @?= Black
  statPosition stat2 @?= Shogi.Position.fromLists ([ ((F5, R7), pawn False White)
                                                   , ((F5, R9), king Black)
                                                   , ((F5, R8), gold White)
                                                   ],
                                                   [])
  statClock    stat2 @?= GameClock.countdown 1 White clock1
  statTime     stat2 @?= time2

  step "手順"
  let moves' = shogiMoves shogi2
  let move1  = head moves'
  length moves' @?= 1
  moveColor    move1 @?= White
  moveMoveType move1 @?= dropPiece (gold White) (F5, R8)
  moveSec      move1 @?= 1
  moveTime     move1 @?= time2

  return ()

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
             歩             R3
 金
--}
後手詰み :: (String -> IO ()) -> IO ()
後手詰み step = do
  time1 <- getCurrentTime
  let position1 = Shogi.Position.fromLists ([ ((F5, R1), king White)
                                            , ((F5, R3), pawn False Black)
                                            ],
                                            [ gold Black
                                            ])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi Black position1 clock1 time1

  step "先手52金"
  let time2  = addUTCTime (fromInteger 1) time1
  let shogi2 = fromJust $ Shogi.move (dropPiece (gold Black) (F5, R2)) 1 time2 shogi1

  step "結果"
  shogiResult shogi2 @?= Win Black Checkmate

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 2

  step "局面1"
  let stat1  = stats !! 1
  statColor    stat1 @?= Black
  statPosition stat1 @?= Shogi.Position.fromLists ([ ((F5, R1), king White)
                                                   , ((F5, R3), pawn False Black)
                                                   ],
                                                   [ gold Black
                                                   ])
  statClock    stat1 @?= clock1
  statTime     stat1 @?= time1

  step "局面2"
  let stat2 = stats !! 0
  statColor    stat2 @?= White
  statPosition stat2 @?= Shogi.Position.fromLists ([ ((F5, R1), king White)
                                                   , ((F5, R3), pawn False Black)
                                                   , ((F5, R2), gold Black)
                                                   ],
                                                   [])
  statClock    stat2 @?= GameClock.countdown 1 Black clock1
  statTime     stat2 @?= time2

  step "手順"
  let moves' = shogiMoves shogi2
  let move1  = head moves'
  length moves' @?= 1
  moveColor    move1 @?= Black
  moveMoveType move1 @?= dropPiece (gold Black) (F5, R2)
  moveSec      move1 @?= 1
  moveTime     move1 @?= time2

  return ()

先手時間切れ :: (String -> IO ()) -> IO ()
先手時間切れ step = do
  time1 <- getCurrentTime
  let position1 = Shogi.Position.fromLists ([ ((F5, R1), king White)
                                            , ((F5, R9), king Black)
                                            ],
                                            [ pawn False White
                                            , pawn False White
                                            , pawn False Black
                                            , pawn False Black
                                            ])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi Black position1 clock1 time1

  step "先手53歩"
  let time2  = addUTCTime (fromInteger 60 * 10) time1
  let shogi2 = fromJust $ Shogi.move (dropPiece (pawn False Black) (F5, R3)) (60 * 10) time2 shogi1

  step "時間切れ"
  shogiResult shogi2 @?= Win White TimeForfeit

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 1

  let stat1  = stats !! 0
  statColor    stat1 @?= Black
  statPosition stat1 @?= Shogi.Position.fromLists ([ ((F5, R1), king White)
                                                   , ((F5, R9), king Black)
                                                   ],
                                                   [ pawn False White
                                                   , pawn False White
                                                   , pawn False Black
                                                   , pawn False Black
                                                   ])
  statClock stat1 @?= GameClock.countdown (60 * 10) Black clock1
  statTime  stat1 @?= time1

  step "手順"
  shogiMoves shogi2 @?= []

  return ()

後手時間切れ :: (String -> IO ()) -> IO ()
後手時間切れ step = do
  time1 <- getCurrentTime
  let position1 = Shogi.Position.fromLists ([ ((F5, R1), king White)
                                            , ((F5, R9), king Black)
                                            ],
                                            [ pawn False White
                                            , pawn False White
                                            , pawn False Black
                                            , pawn False Black
                                            ])
  let clock1 = clock $ GameClock.Clock.suddenDeath 1 (60 * 10)
  let shogi1 = Shogi.initShogi White position1 clock1 time1

  step "先手53歩"
  let time2  = addUTCTime (fromInteger 60 * 10) time1
  let shogi2 = fromJust $ Shogi.move (dropPiece (pawn False White) (F5, R7)) (60 * 10) time2 shogi1

  step "時間切れ"
  shogiResult shogi2 @?= Win Black TimeForfeit

  step "局面"
  let stats = shogiStats shogi2
  length stats @?= 1

  let stat1  = stats !! 0
  statColor    stat1 @?= White
  statPosition stat1 @?= Shogi.Position.fromLists ([ ((F5, R1), king White)
                                                   , ((F5, R9), king Black)
                                                   ],
                                                   [ pawn False White
                                                   , pawn False White
                                                   , pawn False Black
                                                   , pawn False Black
                                                   ])
  statClock stat1 @?= GameClock.countdown (60 * 10) White clock1
  statTime  stat1 @?= time1

  step "手順"
  shogiMoves shogi2 @?= []

  return ()
