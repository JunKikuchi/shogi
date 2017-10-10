module ShogiTest.MoveTest.ResignTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Resign"
  [ testCaseSteps "先手投了"         先手投了
  , testCaseSteps "後手投了"         後手投了
  , testCaseSteps "先手投了時間切れ" 先手投了時間切れ
  , testCaseSteps "後手投了時間切れ" 後手投了時間切れ
  ]

先手投了 :: (String -> IO ()) -> IO ()
先手投了 step = do
  step ""
  assertBool "未実装" False
  return ()

後手投了 :: (String -> IO ()) -> IO ()
後手投了 step = do
  step ""
  assertBool "未実装" False
  return ()

先手投了時間切れ :: (String -> IO ()) -> IO ()
先手投了時間切れ step = do
  step ""
  assertBool "未実装" False
  return ()

後手投了時間切れ :: (String -> IO ()) -> IO ()
後手投了時間切れ step = do
  step ""
  assertBool "未実装" False
  return ()
