module ShogiTest.BoardTest (tests) where

import qualified ShogiTest.BoardTest.DropsTest as DropsTest
import qualified ShogiTest.BoardTest.DropTest  as DropTest
import qualified ShogiTest.BoardTest.MovesTest as MovesTest
import qualified ShogiTest.BoardTest.MoveTest  as MoveTest
import           Test.Tasty

tests :: TestTree
tests = testGroup "Board"
  [ MoveTest.tests
  , DropTest.tests
  , MovesTest.tests
  , DropsTest.tests
  ]
