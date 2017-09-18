import Test.Tasty

import ShogiBoardTest.PieceTest     as PieceTest
import ShogiBoardTest.BoardTest     as BoardTest
import ShogiBoardTest.CheckTest     as CheckTest
import ShogiBoardTest.CheckmateTest as CheckmateTest
import ShogiBoardTest.MovesTest     as MovesTest

main :: IO ()
main = defaultMain $ testGroup "ShogiBoard"
  [ PieceTest.tests
  , BoardTest.tests
  , CheckTest.tests
  , CheckmateTest.tests
  , MovesTest.tests
  ]
