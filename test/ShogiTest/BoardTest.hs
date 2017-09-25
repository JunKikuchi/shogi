module ShogiTest.BoardTest (tests) where

import Test.Tasty
import qualified ShogiTest.BoardTest.MoveTest  as MoveTest
import qualified ShogiTest.BoardTest.DropTest  as DropTest
import qualified ShogiTest.BoardTest.MovesTest as MovesTest
import qualified ShogiTest.BoardTest.DropsTest as DropsTest

tests :: TestTree
tests = testGroup "Board"
  [ MoveTest.tests
  , DropTest.tests
  , MovesTest.tests
  , DropsTest.tests
  ]
