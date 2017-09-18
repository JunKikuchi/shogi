import Test.Tasty

import ShogiBoardTest.PieceTest     as PieceTest
import ShogiBoardTest.CheckTest     as CheckTest
import ShogiBoardTest.CheckmateTest as CheckmateTest

main :: IO ()
main = defaultMain $ testGroup "ShogiBoard"
  [ PieceTest.tests
  , CheckTest.tests
  , CheckmateTest.tests
  ]
