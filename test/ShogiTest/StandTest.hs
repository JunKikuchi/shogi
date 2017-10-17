module ShogiTest.StandTest (tests) where

import Prelude hiding (take)
import Test.Tasty
import Test.Tasty.HUnit
import Shogi.Stand
import Shogi.Piece
import Shogi.Color

tests :: TestTree
tests = testGroup "Stand"
  [ testCase "駒台はソート済み"         $ toList (fromList [king White, king Black, pawn False Black]) @?= [pawn False Black, king Black, king White]
  , testCase "手番の持ち駒リスト"       $ pieces Black (fromList [king White, king Black, pawn False Black]) @?= [pawn False Black, king Black]
  , testCase "駒台に駒を載せる"         $ put (king Black) (fromList []) @?= fromList [king White]
  , testCase "駒台から駒を取り除く"     $ take (pawn False Black) (fromList [pawn False Black, pawn False Black]) @?= Just (fromList [pawn False Black])
  , testCase "駒台から駒を取り除けない" $ take (rook False Black) (fromList [pawn False Black, pawn False Black]) @?= Nothing
  , testCase "駒台に駒がある"           $ included (pawn False Black) (fromList [pawn False Black, pawn False Black]) @?= True
  , testCase "駒台に駒がない"           $ included (pawn False White) (fromList [pawn False Black, pawn False Black]) @?= False
  ]
