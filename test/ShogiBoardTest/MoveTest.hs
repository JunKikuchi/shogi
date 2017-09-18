module ShogiBoardTest.MoveTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import ShogiBoard
import ShogiBoard.Board as Board hiding (move)
import ShogiBoard.Stand as Stand
import ShogiBoard.Piece
import ShogiBoard.Square
import ShogiBoard.Color

tests :: TestTree
tests = testGroup "move"
    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R8
                 王             R9
    --}
  [ testCase "先手王将" $ move (F5, R9) ((F5, R8), False) Black (ShogiBoard (Board.fromList [((F5, R9), king Black)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F5, R8), king Black)]) (Stand.fromList []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
    --}
  , testCase "後手王将" $ move (F5, R1) ((F5, R2), False) White (ShogiBoard (Board.fromList [((F5, R1), king White)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F5, R2), king White)]) (Stand.fromList []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R3
                 銀             R4
    --}
  , testCase "先手銀将成り" $ move (F5, R4) ((F5, R3), True) Black (ShogiBoard (Board.fromList [((F5, R4), silver False Black)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F5, R3), silver True Black)]) (Stand.fromList []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V銀             R6
                                R7
    --}
  , testCase "後手銀将成り" $ move (F5, R6) ((F5, R7), True) White (ShogiBoard (Board.fromList [((F5, R6), silver False White)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F5, R7), silver True White)]) (Stand.fromList []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 飛             R5
    --}
  , testCase "先手飛車成れない" $ move (F5, R5) ((F5, R4), True) Black (ShogiBoard (Board.fromList [((F5, R5), rook False Black)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R5
                                R6
    --}
  , testCase "後手飛車成れない" $ move (F5, R5) ((F5, R6), True) White (ShogiBoard (Board.fromList [((F5, R5), rook False White)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                 と             R5
    --}
  , testCase "先手と金もう一回成れない" $ move (F5, R5) ((F4, R5), True) Black (ShogiBoard (Board.fromList [((F5, R5), pawn True Black)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                Vと             R5
    --}
  , testCase "後手と金もう一回成れない" $ move (F5, R5) ((F4, R5), True) White (ShogiBoard (Board.fromList [((F5, R5), pawn True White)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
             V歩                R4
                 角             R5
    --}
  , testCase "先手角行先手の歩取り" $ move (F5, R5) ((F6, R4), False) Black (ShogiBoard (Board.fromList [((F5, R5), bishop False Black), ((F6, R4), pawn False White)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F6, R4), bishop False Black)]) (Stand.fromList [pawn False Black]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V角             R5
              歩                R6
    --}
  , testCase "後手角行先手の歩取り" $ move (F5, R5) ((F6, R6), False) White (ShogiBoard (Board.fromList [((F5, R5), bishop False White), ((F6, R6), pawn False Black)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F6, R6), bishop False White)]) (Stand.fromList [pawn False White]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
             V金                R3
                                R4
                 桂             R5
    --}
  , testCase "先手桂馬後手の金将取って成り" $ move (F5, R5) ((F6, R3), True) Black (ShogiBoard (Board.fromList [((F5, R5), knight False Black), ((F6, R3), gold White)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F6, R3), knight True Black)]) (Stand.fromList [gold Black]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V桂             R5
                                R6
              金                R7
    --}
  , testCase "後手桂馬先手の金将取って成り" $ move (F5, R5) ((F6, R7), True) White (ShogiBoard (Board.fromList [((F5, R5), knight False White), ((F6, R7), gold Black)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F6, R7), knight True White)]) (Stand.fromList [gold White]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 王             R5
                                R6
    --}
  , testCase "先手移動元の駒が無い" $ move (F5, R9) ((F5, R8), False) Black (ShogiBoard (Board.fromList [((F5, R5), king Black)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                V王             R5
                                R6
    --}
  , testCase "後手移動元の駒が無い" $ move (F5, R1) ((F5, R2), False) White (ShogiBoard (Board.fromList [((F5, R5), king White)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 王             R5
                                R6
    --}
  , testCase "先手王将移動先は動けない升目" $ move (F5, R5) ((F5, R3), False) Black (ShogiBoard (Board.fromList [((F5, R5), king Black)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                V王             R5
                                R6
    --}
  , testCase "後手王将移動先は動けない升目" $ move (F5, R5) ((F5, R7), False) White (ShogiBoard (Board.fromList [((F5, R5), king White)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 王             R5
                                R6
    --}
  , testCase "先手王将移動元の駒は後手の駒" $ move (F5, R5) ((F5, R4), False) Black (ShogiBoard (Board.fromList [((F5, R5), king White)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                V王             R5
                                R6
    --}
  , testCase "後手王将移動元の駒は先手の駒" $ move (F5, R5) ((F5, R6), False) White (ShogiBoard (Board.fromList [((F5, R5), king Black)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                                R8
                 王             R9
    --}
  , testCase "先手王将王手回避" $ move (F5, R9) ((F4, R9), False) Black (ShogiBoard (Board.fromList [((F5, R9), king Black), ((F5, R7), rook False White)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F4, R9), king Black), ((F5, R7), rook False White)]) (Stand.fromList []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
                 飛             R3
    --}
  , testCase "後手王将王手回避" $ move (F5, R1) ((F6, R1), False) White (ShogiBoard (Board.fromList [((F5, R1), king White), ((F5, R3), rook False Black)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F6, R1), king White), ((F5, R3), rook False Black)]) (Stand.fromList []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                                R8
                 王             R9
    --}
  , testCase "先手王将王手回避してない" $ move (F5, R9) ((F5, R8), False) Black (ShogiBoard (Board.fromList [((F5, R9), king Black), ((F5, R7), rook False White)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
                 飛             R3
    --}
  , testCase "後手王将王手回避してない" $ move (F5, R1) ((F5, R2), False) White (ShogiBoard (Board.fromList [((F5, R1), king White), ((F5, R3), rook False Black)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                 金             R8
                 王             R9
    --}
  , testCase "先手金将王手回避してない" $ move (F5, R8) ((F4, R8), False) Black (ShogiBoard (Board.fromList [((F5, R9), king Black), ((F5, R8), gold Black), ((F5, R7), rook False White)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                V金             R2
                 飛             R3
    --}
  , testCase "後手金将王手回避してない" $ move (F5, R2) ((F6, R2), False) White (ShogiBoard (Board.fromList [((F5, R1), king White), ((F5, R2), king White), ((F5, R3), rook False Black)]) (Stand.fromList [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                 金             R8
                 王             R9
    --}
  , testCase "先手金将駒を取って王手回避" $ move (F5, R8) ((F5, R7), False) Black (ShogiBoard (Board.fromList [((F5, R9), king Black), ((F5, R8), gold Black), ((F5, R7), rook False White)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F5, R9), king Black), ((F5, R7), gold Black)]) (Stand.fromList [rook False Black]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                V金             R2
                 飛             R3
    --}
  , testCase "後手金将駒を取って王手回避" $ move (F5, R2) ((F5, R3), False) White (ShogiBoard (Board.fromList [((F5, R1), king White), ((F5, R2), gold White), ((F5, R3), rook False Black)]) (Stand.fromList [])) @?= Just (ShogiBoard (Board.fromList [((F5, R1), king White), ((F5, R3), gold White)]) (Stand.fromList [rook False White]))
  ]
