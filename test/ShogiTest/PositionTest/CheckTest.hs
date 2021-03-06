module ShogiTest.PositionTest.CheckTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Shogi.Position
import Shogi.Color
import Shogi.Square
import Shogi.Piece

tests :: TestTree
tests = testGroup "check"
  [ testCase "後手王手" $ check Black 後手王手 @?= True
  , testCase "先手王手" $ check White 先手王手 @?= True
  ]

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V金             R8
             王             R9
--}
後手王手 :: Position
後手王手 = fromLists (board, stand)
  where
    board = [ ((F5, R8), gold White)
            , ((F5, R9), king Black)
            ]
    stand = []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
             金             R2
--}
先手王手 :: Position
先手王手 = fromLists (board, stand)
  where
    board = [ ((F5, R1), king White)
            , ((F5, R2), gold Black)
            ]
    stand = []
