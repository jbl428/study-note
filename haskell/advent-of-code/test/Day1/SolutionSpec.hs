module Day1.SolutionSpec where

import Day1.Solution (solve)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Day 1" $ do
    let input = unlines ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]
    it "should success" $ do
      solve input `shouldBe` 5
