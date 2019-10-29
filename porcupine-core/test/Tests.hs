import Test.Tasty
import qualified Test.Porcupine.PTask as PTask


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ PTask.tests
  ]
