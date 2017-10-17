module ShogiTest.MoveTest.MovePieceTest (tests) where

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
tests = testGroup "MovePiece"
  [ testCaseSteps "駒を動かす"    駒を動かす
  , testCaseSteps "先手詰み"      先手詰み
  , testCaseSteps "後手詰み"      後手詰み
  , testCaseSteps "先手詰み回避"  先手詰み回避
  , testCaseSteps "後手詰み回避"  後手詰み回避
  , testCaseSteps "先手時間切れ"  先手時間切れ
  , testCaseSteps "後手時間切れ"  後手時間切れ
  ]

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
            V歩             R3
                            R4
                            R5
                            R6
             歩             R7
                            R8
             王             R9
--}
駒を動かす :: (String -> IO ()) -> IO ()
駒を動かす step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False White)
                                   , ((F5, R7), pawn False Black)
                                   , ((F5, R9), king Black)
                                   ], [])
  let shogi1 = shogi Black pos1 clock1 time1

  step "[先手56歩]"
  let time2  = addUTCTime 1 time1
  let move2  = movePiece (F5, R7) ((F5, R6), False)
  let shogi2 = fromJust $ move move2 1 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 1 Black clock1
  statColor    stat2 @?= White
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False White)
                                             , ((F5, R6), pawn False Black)
                                             , ((F5, R9), king Black)
                                             ], [])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move Black move2 1 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "対局中"
  shogiResult shogi2 @?= InProgress

  step "[後手54歩]"
  let time3  = addUTCTime 1 time2
  let move3  = movePiece (F5, R3) ((F5, R4), False)
  let shogi3 = fromJust $ move move3 1 time3 shogi2

  step "局面"
  let stat3  = shogiStat shogi3
  let clock3 = GameClock.countdown 1 White clock2
  statColor    stat3 @?= Black
  statPosition stat3 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R4), pawn False White)
                                             , ((F5, R6), pawn False Black)
                                             , ((F5, R9), king Black)
                                             ], [])
  statClock    stat3 @?= clock3
  statTime     stat3 @?= time3

  step "手順"
  let moves3 = shogiMoves shogi3
  length moves3 @?= 3
  moves3 !! 0 @?= Move White move3 1 time3 stat3
  moves3 !! 1 @?= shogiMoves shogi2 !! 0
  moves3 !! 2 @?= shogiMoves shogi2 !! 1

  step "対局中"
  shogiResult shogi3 @?= InProgress

  step "[先手49王]"
  let time4  = addUTCTime 1 time3
  let move4  = movePiece (F5, R9) ((F4, R9), False)
  let shogi4 = fromJust $ move move4 1 time4 shogi3

  step "局面"
  let stat4  = shogiStat shogi4
  let clock4 = GameClock.countdown 1 Black clock3
  statColor    stat4 @?= White
  statPosition stat4 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R4), pawn False White)
                                             , ((F5, R6), pawn False Black)
                                             , ((F4, R9), king Black)
                                             ], [])
  statClock    stat4 @?= clock4
  statTime     stat4 @?= time4

  step "手順"
  let moves4 = shogiMoves shogi4
  length moves4 @?= 4
  moves4 !! 0 @?= Move Black move4 1 time4 stat4
  moves4 !! 1 @?= shogiMoves shogi3 !! 0
  moves4 !! 2 @?= shogiMoves shogi3 !! 1
  moves4 !! 3 @?= shogiMoves shogi3 !! 2

  step "対局中"
  shogiResult shogi4 @?= InProgress

  step "[後手53歩には動かせない]"
  let time4' = addUTCTime 1 time4
  let move4' = movePiece (F5, R4) ((F5, R3), False)
  move move4' 1 time4' shogi4 @?= Nothing

  step "[先手手番違い]"
  let time4'' = addUTCTime 1 time4
  let move4'' = movePiece (F5, R6) ((F5, R5), False)
  move move4'' 1 time4'' shogi4 @?= Nothing

  step "[後手55歩]"
  let time5  = addUTCTime 1 time4
  let move5  = movePiece (F5, R4) ((F5, R5), False)
  let shogi5 = fromJust $ move move5 1 time5 shogi4

  step "局面"
  let stat5  = shogiStat shogi5
  let clock5 = GameClock.countdown 1 White clock4
  statColor    stat5 @?= Black
  statPosition stat5 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R5), pawn False White)
                                             , ((F5, R6), pawn False Black)
                                             , ((F4, R9), king Black)
                                             ], [])
  statClock    stat5 @?= clock5
  statTime     stat5 @?= time5

  step "手順"
  let moves5 = shogiMoves shogi5
  length moves5 @?= 5
  moves5 !! 0 @?= Move White move5 1 time5 stat5
  moves5 !! 1 @?= shogiMoves shogi4 !! 0
  moves5 !! 2 @?= shogiMoves shogi4 !! 1
  moves5 !! 3 @?= shogiMoves shogi4 !! 2
  moves5 !! 4 @?= shogiMoves shogi4 !! 3

  step "対局中"
  shogiResult shogi5 @?= InProgress

  step "[先手54歩には動かせない]"
  let time5'  = addUTCTime 1 time5
  let move5'  = movePiece (F5, R6) ((F5, R4), False)
  move move5' 1 time5' shogi5 @?= Nothing

  step "[後手手番違い]"
  let time5''  = addUTCTime 1 time5
  let move5''  = movePiece (F5, R5) ((F5, R6), False)
  move move5'' 1 time5'' shogi5 @?= Nothing

  return ()

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
             歩 金          R3
                            R4
                            R5
                            R6
            V歩V金          R7
                            R8
             王             R9
--}
先手詰み :: (String -> IO ()) -> IO ()
先手詰み step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F4, R3), gold Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F4, R7), gold White)
                                   , ((F5, R9), king Black)
                                   ], [])
  let shogi1 = shogi White pos1 clock1 time1

  step "後手58金"
  let time2  = addUTCTime 1 time1
  let move2  = movePiece (F4, R7) ((F5, R8), False)
  let shogi2 = fromJust $ move move2 1 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 1 White clock1
  statColor    stat2 @?= Black
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False Black)
                                             , ((F4, R3), gold Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F5, R8), gold White)
                                             , ((F5, R9), king Black)
                                             ], [])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move White move2 1 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "対局終了"
  shogiResult shogi2 @?= Win White Checkmate

  return ()

後手詰み :: (String -> IO ()) -> IO ()
後手詰み step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F4, R3), gold Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F4, R7), gold White)
                                   , ((F5, R9), king Black)
                                   ], [])
  let shogi1 = shogi Black pos1 clock1 time1

  step "先手52金"
  let time2  = addUTCTime 1 time1
  let move2  = movePiece (F4, R3) ((F5, R2), False)
  let shogi2 = fromJust $ move move2 1 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 1 Black clock1
  statColor    stat2 @?= White
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False Black)
                                             , ((F5, R2), gold Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F4, R7), gold White)
                                             , ((F5, R9), king Black)
                                             ], [])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move Black move2 1 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "対局終了"
  shogiResult shogi2 @?= Win Black Checkmate

  return ()

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
             金             R3
                            R4
                            R5
                            R6
            V金             R7
            V歩             R8
             王             R9
--}
先手詰み回避 :: (String -> IO ()) -> IO ()
先手詰み回避 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), gold Black)
                                   , ((F5, R7), gold White)
                                   , ((F5, R8), pawn False White)
                                   , ((F5, R9), king Black)
                                   ], [])
  let shogi1 = shogi Black pos1 clock1 time1

  step "先手52金は王手回避していないので指せない"
  let time2 = addUTCTime 1 time1
  let move2 = movePiece (F5, R3) ((F5, R2), False)
  move move2 1 time2 shogi1 @?= Nothing

  return ()

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
             歩             R2
             金             R3
                            R4
                            R5
                            R6
            V金             R7
                            R8
             王             R9
--}
後手詰み回避 :: (String -> IO ()) -> IO ()
後手詰み回避 step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 (60 * 10)
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R2), pawn False Black)
                                   , ((F5, R3), gold Black)
                                   , ((F5, R7), gold White)
                                   , ((F5, R9), king Black)
                                   ], [])
  let shogi1 = shogi White pos1 clock1 time1

  step "後手58金は王手回避していないので指せない"
  let time2 = addUTCTime 1 time1
  let move2 = movePiece (F5, R7) ((F5, R8), False)
  move move2 1 time2 shogi1 @?= Nothing

  return ()

先手時間切れ :: (String -> IO ()) -> IO ()
先手時間切れ step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F4, R3), gold Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F4, R7), gold White)
                                   , ((F5, R9), king Black)
                                   ], [])
  let shogi1 = shogi Black pos1 clock1 time1

  step "先手52金"
  let time2  = addUTCTime 10 time1
  let move2  = movePiece (F4, R3) ((F5, R2), False)
  let shogi2 = fromJust $ move move2 10 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 10 Black clock1
  statColor    stat2 @?= Black
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False Black)
                                             , ((F4, R3), gold Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F4, R7), gold White)
                                             , ((F5, R9), king Black)
                                             ], [])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move Black TimeIsUp 10 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "対局終了"
  shogiResult shogi2 @?= Win White TimeForfeit

  return ()

後手時間切れ :: (String -> IO ()) -> IO ()
後手時間切れ step = do
  time1 <- getCurrentTime
  let clock1 = clock $ suddenDeath 1 10
  let pos1   = Position.fromLists ([ ((F5, R1), king White)
                                   , ((F5, R3), pawn False Black)
                                   , ((F4, R3), gold Black)
                                   , ((F5, R7), pawn False White)
                                   , ((F4, R7), gold White)
                                   , ((F5, R9), king Black)
                                   ], [])
  let shogi1 = shogi White pos1 clock1 time1

  step "後手58金"
  let time2  = addUTCTime 10 time1
  let move2  = movePiece (F4, R7) ((F5, R8), False)
  let shogi2 = fromJust $ move move2 10 time2 shogi1

  step "局面"
  let stat2  = shogiStat shogi2
  let clock2 = GameClock.countdown 10 White clock1
  statColor    stat2 @?= White
  statPosition stat2 @?= Position.fromLists ([ ((F5, R1), king White)
                                             , ((F5, R3), pawn False Black)
                                             , ((F4, R3), gold Black)
                                             , ((F5, R7), pawn False White)
                                             , ((F4, R7), gold White)
                                             , ((F5, R9), king Black)
                                             ], [])
  statClock    stat2 @?= clock2
  statTime     stat2 @?= time2

  step "手順"
  let moves2 = shogiMoves shogi2
  length moves2 @?= 2
  moves2 !! 0 @?= Move White TimeIsUp 10 time2 stat2
  moves2 !! 1 @?= shogiMoves shogi1 !! 0

  step "対局終了"
  shogiResult shogi2 @?= Win Black TimeForfeit

  return ()
