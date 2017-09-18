module ShogiBoardTest.CheckTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import ShogiBoard
import ShogiBoard.Color
import ShogiBoard.Board as Board hiding (check)
import ShogiBoard.Stand as Stand
import ShogiBoard.Square
import ShogiBoard.Piece

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
後手王手 :: ShogiBoard
後手王手 = ShogiBoard board stand
  where
    board = Board.fromList
      [ ((F5, R8), gold White)
      , ((F5, R9), king Black)
      ]
    stand = Stand.fromList []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
             金             R2
--}
先手王手 :: ShogiBoard
先手王手 = ShogiBoard board stand
  where
    board = Board.fromList
      [ ((F5, R1), king White)
      , ((F5, R2), gold Black)
      ]
    stand = Stand.fromList []
