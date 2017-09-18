module ShogiBoardTest.BoardTest (tests) where

import Test.Tasty
import qualified ShogiBoardTest.BoardTest.MoveTest  as MoveTest
import qualified ShogiBoardTest.BoardTest.DropTest  as DropTest
import qualified ShogiBoardTest.BoardTest.MovesTest as MovesTest
import qualified ShogiBoardTest.BoardTest.DropsTest as DropsTest

tests :: TestTree
tests = testGroup "Board"
  [ MoveTest.tests
  , DropTest.tests
  , MovesTest.tests
  , DropsTest.tests
  ]
