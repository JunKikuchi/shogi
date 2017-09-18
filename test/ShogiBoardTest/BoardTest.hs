module ShogiBoardTest.BoardTest (tests) where

import Test.Tasty
import qualified ShogiBoardTest.BoardTest.MoveTest  as MoveTest
import qualified ShogiBoardTest.BoardTest.MovesTest as MovesTest

tests :: TestTree
tests = testGroup "Board"
  [ MoveTest.tests
  , MovesTest.tests
  ]
