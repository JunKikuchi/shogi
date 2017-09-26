module ShogiTest.PositionTest.DropTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (drop)
import Shogi.Position
import Shogi.Piece
import Shogi.Square
import Shogi.Color

tests :: TestTree
tests = testGroup "drop"
    {--
     V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
     歩歩
    --}
  [ testCase "先手歩兵" $ drop (pawn False Black) (F5, R5) (fromLists ([], [pawn False Black, pawn False Black, pawn False White])) @?= Just (fromLists ([((F5, R5), pawn False Black)], [pawn False Black, pawn False White]))

    {--
     V歩 V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
     歩
    --}
  , testCase "後手歩兵" $ drop (pawn False White) (F5, R5) (fromLists ([], [pawn False White, pawn False Black, pawn False White])) @?= Just (fromLists ([((F5, R5), pawn False White)], [pawn False Black, pawn False White]))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
     歩
    --}
  , testCase "先手歩兵成り駒は指せない" $ drop (pawn True Black) (F5, R5) (fromLists ([], [pawn False Black])) @?= Nothing

    {--
     V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
    --}
  , testCase "後手歩兵成り駒は指せない" $ drop (pawn True White) (F5, R5) (fromLists ([], [pawn False White])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
    --}
  , testCase "先手持ち駒なし" $ drop (pawn False Black) (F5, R5) (fromLists ([], [])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
    --}
  , testCase "後手持ち駒なし" $ drop (pawn False White) (F5, R5) (fromLists ([], [])) @?= Nothing

    {--
     V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
     金
    --}
  , testCase "先手持ち駒にない" $ drop (pawn False Black) (F5, R5) (fromLists ([], [pawn False White, gold Black])) @?= Nothing

    {--
     V金
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                                R5
     歩
    --}
  , testCase "後手持ち駒にない" $ drop (pawn False White) (F5, R5) (fromLists ([], [gold White, pawn False Black])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                 歩             R5
     歩
    --}
  , testCase "先手歩兵自分の駒があって指せない" $ drop (pawn False Black) (F5, R5) (fromLists ([((F5, R5), pawn False Black)], [pawn False Black])) @?= Nothing

    {--
     V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R5
    --}
  , testCase "後手歩兵自分の駒があって指せない" $ drop (pawn False White) (F5, R5) (fromLists ([((F5, R5), pawn False White)], [pawn False White])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V歩             R5
     歩
    --}
  , testCase "先手歩兵相手の駒があって指せない" $ drop (pawn False Black) (F5, R5) (fromLists ([((F5, R5), pawn False White)], [pawn False Black])) @?= Nothing

    {--
     V歩
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                 歩             R5
    --}
  , testCase "後手歩兵相手の駒があって指せない" $ drop (pawn False White) (F5, R5) (fromLists ([((F5, R5), pawn False Black)], [pawn False White])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                                R8
                 王             R9
     歩
    --}
  , testCase "先手歩兵王手回避" $ drop (pawn False Black) (F5, R8) (fromLists ([((F5, R7), rook False White), ((F5, R9), king Black)], [pawn False Black])) @?= Just (fromLists ([((F5, R7), rook False White), ((F5, R8), pawn False Black), ((F5, R9), king Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
                 飛             R3
     歩
    --}
  , testCase "後手歩兵王手回避" $ drop (pawn False White) (F5, R2) (fromLists ([((F5, R1), king White), ((F5, R3), rook False Black)], [pawn False White])) @?= Just (fromLists ([((F5, R1), king White), ((F5, R2), pawn False White), ((F5, R3), rook False Black)], []))

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V飛             R7
                                R8
                 王             R9
     歩
    --}
  , testCase "先手歩兵王手回避してない" $ drop (pawn False Black) (F4, R8) (fromLists ([((F5, R7), rook False White), ((F5, R9), king Black)], [pawn False Black])) @?= Nothing

    {--
     F9 F8 F7 F6 F5 F4 F3 F2 F1
                V王             R1
                                R2
                 飛             R3
     歩
    --}
  , testCase "後手歩兵王手回避してない" $ drop (pawn False White) (F6, R2) (fromLists ([((F5, R1), king White), ((F5, R3), rook False Black)], [pawn False White])) @?= Nothing
  ]
