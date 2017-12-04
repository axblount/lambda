import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Control.Exception (evaluate)

import Lambda

main :: IO ()
main = hspec $ do
  describe "Lambda" $ do
    it "parses valid statements" $ do
    it "evaulates things correctly" $ do
      shouldBe (evalLambda (Index 0)) (Index 0)
      shouldBe (evalLambda $ unsafeParseString "(\\x.x) y") (Index 0)