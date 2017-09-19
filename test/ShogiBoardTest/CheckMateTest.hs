module ShogiBoardTest.CheckmateTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import ShogiBoard
import ShogiBoard.Color
import ShogiBoard.Square
import ShogiBoard.Piece

tests :: TestTree
tests = testGroup "checkmate"
  [ testCase "先手頭金で詰み"       $ checkmate Black 先手頭金     @?= True
  , testCase "後手頭金で詰み"       $ checkmate White 後手頭金     @?= True
  , testCase "先手頭金で詰まず"     $ checkmate Black 先手頭銀     @?= False
  , testCase "後手頭金で詰まず"     $ checkmate White 後手頭銀     @?= False
  , testCase "先手合駒なしで詰み"   $ checkmate Black 先手合駒なし @?= True
  , testCase "後手合駒なしで詰み"   $ checkmate White 後手合駒なし @?= True
  , testCase "先手合駒ありで詰まず" $ checkmate Black 先手合駒あり @?= False
  , testCase "後手合駒ありで詰まず" $ checkmate White 後手合駒あり @?= False
  , testCase "先手駒を取って詰まず" $ checkmate Black 先手駒取り   @?= False
  , testCase "後手駒を取って詰まず" $ checkmate White 後手駒取り   @?= False
  ]

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V歩             R7
            V金             R8
             王             R9
--}
先手頭金 :: ShogiBoard
先手頭金 = ShogiBoard.fromLists board stand
  where
    board = [ ((F5, R7), pawn False White)
            , ((F5, R8), gold White)
            , ((F5, R9), king Black)
            ]
    stand = []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
             金             R2
             歩             R3
--}
後手頭金 :: ShogiBoard
後手頭金 = ShogiBoard.fromLists board stand
  where
    board = [ ((F5, R1), king White)
            , ((F5, R2), gold Black)
            , ((F5, R3), pawn False Black)
            ]
    stand = []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V歩             R7
            V銀             R8
             王             R9
--}
先手頭銀 :: ShogiBoard
先手頭銀 = ShogiBoard.fromLists board stand
  where
    board = [ ((F5, R7), pawn False White)
            , ((F5, R8), silver False White)
            , ((F5, R9), king Black)
            ]
    stand = []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
             銀             R2
             歩             R3
--}
後手頭銀 :: ShogiBoard
後手頭銀 = ShogiBoard.fromLists board stand
  where
    board = [ ((F5, R1), king White)
            , ((F5, R2), silver False Black)
            , ((F5, R3), pawn False Black)
            ]
    stand = []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
         V香V香V香          R7
                            R8
             王             R9
--}
先手合駒なし :: ShogiBoard
先手合駒なし = ShogiBoard.fromLists board stand
  where
    board = [ ((F6, R7), lance False White)
            , ((F5, R7), lance False White)
            , ((F4, R7), lance False White)
            , ((F5, R9), king Black)
            ]
    stand = []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
          香 香 香          R3
--}
後手合駒なし :: ShogiBoard
後手合駒なし = ShogiBoard.fromLists board stand
  where
    board = [ ((F6, R3), lance False Black)
            , ((F5, R3), lance False Black)
            , ((F4, R3), lance False Black)
            , ((F5, R1), king White)
            ]
    stand = []

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
         V香V香V香          R7
                            R8
             王             R9
 歩
--}
先手合駒あり :: ShogiBoard
先手合駒あり = ShogiBoard.fromLists board stand
  where
    board = [ ((F6, R7), lance False White)
            , ((F5, R7), lance False White)
            , ((F4, R7), lance False White)
            , ((F5, R9), king Black)
            ]
    stand = [pawn False Black]

{--
 歩
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V王             R1
                            R2
          香 香 香          R3
--}
後手合駒あり :: ShogiBoard
後手合駒あり = ShogiBoard.fromLists board stand
  where
    board = [ ((F6, R3), lance False Black)
            , ((F5, R3), lance False Black)
            , ((F4, R3), lance False Black)
            , ((F5, R1), king White)
            ]
    stand = [pawn False White]

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
            V金             R8
             王             R9
--}
先手駒取り :: ShogiBoard
先手駒取り = ShogiBoard.fromLists board stand
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
後手駒取り :: ShogiBoard
後手駒取り = ShogiBoard.fromLists board stand
  where
    board = [ ((F5, R1), king White)
            , ((F5, R2), gold Black)
            ]
    stand = []
