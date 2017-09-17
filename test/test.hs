import Test.Tasty

import ShogiBoardTest.CheckmateTest as CheckmateTest
import ShogiBoardTest.PieceTest     as PieceTest

main :: IO ()
main = defaultMain $ testGroup "ShogiBoard"
  [ CheckmateTest.tests
  , PieceTest.tests
  ]
