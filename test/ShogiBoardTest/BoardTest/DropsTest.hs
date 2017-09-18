module ShogiBoardTest.BoardTest.DropsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "drops"
  [ testCase "未実装" $ False @?= True
  ]
