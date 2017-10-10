module ShogiTest.CountdownTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "countdown"
  [ testCaseSteps "先手の時計を進める" 先手の時計を進める
  , testCaseSteps "後手の時計を進める" 後手の時計を進める
  , testCaseSteps "先手時間切れ"       先手時間切れ
  , testCaseSteps "後手時間切れ"       後手時間切れ
  ]

先手の時計を進める :: (String -> IO ()) -> IO ()
先手の時計を進める step = do
  step ""
  assertBool "未実装" False
  return ()

後手の時計を進める :: (String -> IO ()) -> IO ()
後手の時計を進める step = do
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
