import qualified Test.Porcupine.PTask as PTask
import           Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ PTask.tests
  ]
