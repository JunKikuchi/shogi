module ShogiTest.HirateTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "hirate"
  [ testCaseSteps "平手作成" 平手作成
  ]

平手作成 :: (String -> IO ()) -> IO ()
平手作成 step = do
  step ""
  assertBool "未実装" False
  return ()
