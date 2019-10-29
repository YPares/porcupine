import Test.Tasty
import Test.Tasty.HUnit
import System.TaskPipeline.PTask

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" []
