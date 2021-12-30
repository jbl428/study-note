module Day2.SolutionSpec where

import Day2.Solution (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Day 2" $ do
    let input = unlines ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]
    it "should success" $ do
      solve input `shouldBe` 900