module ShogiBoardTest.DropTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "drop"
  [ testCase "未実装" $ False @?= True
  ]
