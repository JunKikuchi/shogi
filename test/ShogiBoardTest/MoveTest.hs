module ShogiBoardTest.MoveTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "move"
  [ testCase "未実装" $ False @?= True
  ]
