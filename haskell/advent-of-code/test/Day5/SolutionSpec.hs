module Day5.SolutionSpec where

import Day5.Solution (Line (Line), parseInput)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (parse)

inputText :: String
inputText = unlines ["0,9 -> 5,9", "8,0 -> 0,8", "123,456 -> 023,013"]

spec :: Spec
spec = do
  describe "Day 5" $ do
    it "parseInput" $ do
      parse parseInput "" inputText `shouldBe` Right [Line (0, 9) (5, 9), Line (8, 0) (0, 8), Line (123, 456) (23, 13)]