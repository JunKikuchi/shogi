import Test.Tasty

import ShogiBoardTest.CheckmateTest as CheckmateTest

main :: IO ()
main = defaultMain $ testGroup "ShogiBoard"
  [ CheckmateTest.tests ]
