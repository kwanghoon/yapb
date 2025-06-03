module ParserSpec where

import Run

import Test.Hspec

spec = hspec $ do
  describe "parsing yapb/app/parser" $ do
    it "one-line example" $ do
      result <- doProcess False "./app/parser/example/oneline.arith"
      result `shouldBe` "((1 + 2) - ((3 * 4) / 5))"

    it "multi-line example" $ do
      result <- doProcess False "./app/parser/example/multiline.arith"
      result `shouldBe` "(x = 123); (x = (x + 1)); (y = x); (y = (y - ((1 * 2) / 3))); (z = (y = x))"
