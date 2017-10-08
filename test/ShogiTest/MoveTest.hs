module ShogiTest.MoveTest (tests) where

import Test.Tasty
import qualified ShogiTest.MoveTest.MovePieceTest as MovePieceTest

tests :: TestTree
tests = testGroup "move"
  [ MovePieceTest.tests
  ]
