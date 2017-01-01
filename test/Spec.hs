import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy
import qualified HyperEval as HE
import           Test.Tasty
import           Test.Tasty.HUnit

-- Tests

decodeProxy :: A.FromJSON a => Proxy a -> LBS.ByteString -> Either String a
decodeProxy p bs = A.eitherDecode bs `asProxyTypeOf` (Right <$> p)

testRoundTrip :: (A.ToJSON a, A.FromJSON a, Eq a, Show a) => Proxy a -> FilePath -> TestTree
testRoundTrip p path = testCase ("round trip: " ++ path) $ do
  bs <- LBS.readFile path
  case decodeProxy p bs of
    Left e -> fail ("could not decode " ++ path ++ ": " ++ e)
    Right o ->
      let bs' = A.encode o
      in case decodeProxy p bs' of
        Left e' -> fail ("could not decode again" ++ path ++ ": " ++ e')
        Right o' -> o' @?= o

-- Runner

tests :: TestTree
tests = testGroup "Tests"
  [ testRoundTrip (Proxy :: Proxy HE.Notebook) "data/Demo.hhs"
  ]

main :: IO ()
main = defaultMain tests
