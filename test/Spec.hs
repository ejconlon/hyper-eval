import qualified HyperEval as HE

import           Test.Tasty
import           Test.Tasty.HUnit

testSomething :: TestTree
testSomething = testCase "something" $ do
  return ()

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testSomething
  ]

main :: IO ()
main = defaultMain tests
