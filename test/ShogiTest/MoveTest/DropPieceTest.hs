module ShogiTest.MoveTest.DropPieceTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "DropPiece"
  [ testCaseSteps "持ち駒を打つ" 持ち駒を打つ
  , testCaseSteps "先手詰み"     先手詰み
  , testCaseSteps "後手詰み"     後手詰み
  , testCaseSteps "先手時間切れ" 先手時間切れ
  , testCaseSteps "後手時間切れ" 後手時間切れ
  ]

持ち駒を打つ :: (String -> IO ()) -> IO ()
持ち駒を打つ step = do
  step ""
  assertBool "未実装" False
  return ()

先手詰み :: (String -> IO ()) -> IO ()
先手詰み step = do
  step ""
  assertBool "未実装" False
  return ()

後手詰み :: (String -> IO ()) -> IO ()
後手詰み step = do
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
