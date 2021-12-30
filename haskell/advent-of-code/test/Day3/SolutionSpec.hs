module Day3.SolutionSpec where

import Day3.Solution (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Day 3" $ do
    it "one line input" $ do
      solve "1" `shouldBe` 0
      solve "10" `shouldBe` 2
