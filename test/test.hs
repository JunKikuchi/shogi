import Test.Tasty
import ShogiTest

main :: IO ()
main = defaultMain $ testGroup "shogi" [ShogiTest.tests]
