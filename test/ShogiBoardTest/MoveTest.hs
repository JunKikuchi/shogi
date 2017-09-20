module ShogiBoardTest.MoveTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import ShogiBoard
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
  [ testCase "先手王将" $ move ((F5, R9), Black) ((F5, R8), False) (ShogiBoard.fromLists ([((F5, R9), king Black)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R8), king Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
    --}
  , testCase "後手王将" $ move ((F5, R1), White) ((F5, R2), False) (ShogiBoard.fromLists ([((F5, R1), king White)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R2), king White)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R3
                 銀             R4
    --}
  , testCase "先手銀将成り" $ move ((F5, R4), Black) ((F5, R3), True) (ShogiBoard.fromLists ([((F5, R4), silver False Black)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R3), silver True Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V銀             R6
                                R7
    --}
  , testCase "後手銀将成り" $ move ((F5, R6), White) ((F5, R7), True) (ShogiBoard.fromLists ([((F5, R6), silver False White)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R7), silver True White)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                 歩             R2
    --}
  , testCase "先手歩兵成り" $ move ((F5, R2), Black) ((F5, R1), True) (ShogiBoard.fromLists ([((F5, R2), pawn False Black)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R1), pawn True Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R8
                                R9
    --}
  , testCase "後手歩兵成り" $ move ((F5, R8), White) ((F5, R9), True) (ShogiBoard.fromLists ([((F5, R8), pawn False White)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R9), pawn True White)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                                R2
                 桂             R3
    --}
  , testCase "先手桂馬成り" $ move ((F5, R3), Black) ((F6, R1), True) (ShogiBoard.fromLists ([((F5, R3), knight False Black)], [])) @?= Just (ShogiBoard.fromLists ([((F6, R1), knight True Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V桂             R7
                                R8
                                R9
    --}
  , testCase "後手桂馬成り" $ move ((F5, R7), White) ((F4, R9), True) (ShogiBoard.fromLists ([((F5, R7), knight False White)], [])) @?= Just (ShogiBoard.fromLists ([((F4, R9), knight True White)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                                R2
                                R3
                 桂             R4
    --}
  , testCase "先手桂馬成り" $ move ((F5, R4), Black) ((F4, R2), True) (ShogiBoard.fromLists ([((F5, R4), knight False Black)], [])) @?= Just (ShogiBoard.fromLists ([((F4, R2), knight True Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V桂             R6
                                R7
                                R8
                                R9
    --}
  , testCase "後手桂馬成り" $ move ((F5, R6), White) ((F6, R8), True) (ShogiBoard.fromLists ([((F5, R6), knight False White)], [])) @?= Just (ShogiBoard.fromLists ([((F6, R8), knight True White)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                 歩             R2
    --}
  , testCase "先手歩兵成らずはできない" $ move ((F5, R2), Black) ((F5, R1), False) (ShogiBoard.fromLists ([((F5, R2), pawn False Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R8
                                R9
    --}
  , testCase "後手歩兵成らずはできない" $ move ((F5, R8), White) ((F5, R9), False) (ShogiBoard.fromLists ([((F5, R8), pawn False White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                 香             R2
    --}
  , testCase "先手香車成らずはできない" $ move ((F5, R2), Black) ((F5, R1), False) (ShogiBoard.fromLists ([((F5, R2), lance False Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V香             R8
                                R9
    --}
  , testCase "後手香車成らずはできない" $ move ((F5, R8), White) ((F5, R9), False) (ShogiBoard.fromLists ([((F5, R8), lance False White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                                R2
                 桂             R3
    --}
  , testCase "先手桂馬成らずはできない R1" $ move ((F5, R3), Black) ((F6, R1), False) (ShogiBoard.fromLists ([((F5, R3), knight False Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V桂             R7
                                R8
                                R9
    --}
  , testCase "後手桂馬成らずはできない R9" $ move ((F5, R7), White) ((F4, R9), False) (ShogiBoard.fromLists ([((F5, R7), knight False White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R1
                                R2
                                R3
                 桂             R4
    --}
  , testCase "先手桂馬成らずはできない R2" $ move ((F5, R4), Black) ((F4, R2), False) (ShogiBoard.fromLists ([((F5, R4), knight False Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V桂             R6
                                R7
                                R8
                                R9
    --}
  , testCase "後手桂馬成らずはできない R8" $ move ((F5, R6), White) ((F6, R8), False) (ShogiBoard.fromLists ([((F5, R6), knight False White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 飛             R5
    --}
  , testCase "先手飛車成れない" $ move ((F5, R5), Black) ((F5, R4), True) (ShogiBoard.fromLists ([((F5, R5), rook False Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R5
                                R6
    --}
  , testCase "後手飛車成れない" $ move ((F5, R5), White) ((F5, R6), True) (ShogiBoard.fromLists ([((F5, R5), rook False White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                 と             R5
    --}
  , testCase "先手と金もう一回成れない" $ move ((F5, R5), Black) ((F4, R5), True) (ShogiBoard.fromLists ([((F5, R5), pawn True Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                Vと             R5
    --}
  , testCase "後手と金もう一回成れない" $ move ((F5, R5), White) ((F4, R5), True) (ShogiBoard.fromLists ([((F5, R5), pawn True White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
             V歩                R4
                 角             R5
    --}
  , testCase "先手角行先手の歩取り" $ move ((F5, R5), Black) ((F6, R4), False) (ShogiBoard.fromLists ([((F5, R5), bishop False Black), ((F6, R4), pawn False White)], [])) @?= Just (ShogiBoard.fromLists ([((F6, R4), bishop False Black)], [pawn False Black]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V角             R5
              歩                R6
    --}
  , testCase "後手角行先手の歩取り" $ move ((F5, R5), White) ((F6, R6), False) (ShogiBoard.fromLists ([((F5, R5), bishop False White), ((F6, R6), pawn False Black)], [])) @?= Just (ShogiBoard.fromLists ([((F6, R6), bishop False White)], [pawn False White]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
             V金                R3
                                R4
                 桂             R5
    --}
  , testCase "先手桂馬後手の金将取って成り" $ move ((F5, R5), Black) ((F6, R3), True) (ShogiBoard.fromLists ([((F5, R5), knight False Black), ((F6, R3), gold White)], [])) @?= Just (ShogiBoard.fromLists ([((F6, R3), knight True Black)], [gold Black]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V桂             R5
                                R6
              金                R7
    --}
  , testCase "後手桂馬先手の金将取って成り" $ move ((F5, R5), White) ((F6, R7), True) (ShogiBoard.fromLists ([((F5, R5), knight False White), ((F6, R7), gold Black)], [])) @?= Just (ShogiBoard.fromLists ([((F6, R7), knight True White)], [gold White]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 王             R5
                                R6
    --}
  , testCase "先手移動元の駒が無い" $ move ((F5, R9), Black) ((F5, R8), False) (ShogiBoard.fromLists ([((F5, R5), king Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                V王             R5
                                R6
    --}
  , testCase "後手移動元の駒が無い" $ move ((F5, R1), White) ((F5, R2), False) (ShogiBoard.fromLists ([((F5, R5), king White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 王             R5
                                R6
    --}
  , testCase "先手王将移動先は動けない升目" $ move ((F5, R5), Black) ((F5, R3), False) (ShogiBoard.fromLists ([((F5, R5), king Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                V王             R5
                                R6
    --}
  , testCase "後手王将移動先は動けない升目" $ move ((F5, R5), White) ((F5, R7), False) (ShogiBoard.fromLists ([((F5, R5), king White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                 王             R5
                                R6
    --}
  , testCase "先手王将移動元の駒は後手の駒" $ move ((F5, R5), Black) ((F5, R4), False) (ShogiBoard.fromLists ([((F5, R5), king White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R4
                V王             R5
                                R6
    --}
  , testCase "後手王将移動元の駒は先手の駒" $ move ((F5, R5), White) ((F5, R6), False) (ShogiBoard.fromLists ([((F5, R5), king Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                                R8
                 王             R9
    --}
  , testCase "先手王将王手回避" $ move ((F5, R9), Black) ((F4, R9), False) (ShogiBoard.fromLists ([((F5, R9), king Black), ((F5, R7), rook False White)], [])) @?= Just (ShogiBoard.fromLists ([((F4, R9), king Black), ((F5, R7), rook False White)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
                 飛             R3
    --}
  , testCase "後手王将王手回避" $ move ((F5, R1), White) ((F6, R1), False) (ShogiBoard.fromLists ([((F5, R1), king White), ((F5, R3), rook False Black)], [])) @?= Just (ShogiBoard.fromLists ([((F6, R1), king White), ((F5, R3), rook False Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                                R8
                 王             R9
    --}
  , testCase "先手王将王手回避してない" $ move ((F5, R9), Black) ((F5, R8), False) (ShogiBoard.fromLists ([((F5, R9), king Black), ((F5, R7), rook False White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
                 飛             R3
    --}
  , testCase "後手王将王手回避してない" $ move ((F5, R1), White) ((F5, R2), False) (ShogiBoard.fromLists ([((F5, R1), king White), ((F5, R3), rook False Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                 金             R8
                 王             R9
    --}
  , testCase "先手金将王手回避してない" $ move ((F5, R8), Black) ((F4, R8), False) (ShogiBoard.fromLists ([((F5, R9), king Black), ((F5, R8), gold Black), ((F5, R7), rook False White)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                V金             R2
                 飛             R3
    --}
  , testCase "後手金将王手回避してない" $ move ((F5, R2), White) ((F6, R2), False) (ShogiBoard.fromLists ([((F5, R1), king White), ((F5, R2), king White), ((F5, R3), rook False Black)], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                 金             R8
                 王             R9
    --}
  , testCase "先手金将駒を取って王手回避" $ move ((F5, R8), Black) ((F5, R7), False) (ShogiBoard.fromLists ([((F5, R9), king Black), ((F5, R8), gold Black), ((F5, R7), rook False White)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R9), king Black), ((F5, R7), gold Black)], [rook False Black]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                V金             R2
                 飛             R3
    --}
  , testCase "後手金将駒を取って王手回避" $ move ((F5, R2), White) ((F5, R3), False) (ShogiBoard.fromLists ([((F5, R1), king White), ((F5, R2), gold White), ((F5, R3), rook False Black)], [])) @?= Just (ShogiBoard.fromLists ([((F5, R1), king White), ((F5, R3), gold White)], [rook False White]))
  ]
