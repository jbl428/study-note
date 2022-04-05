module Day5.SolutionSpec where

import Day5.Solution (Line (Line), getPoints, parseInput)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (parse)

inputText :: String
inputText = unlines ["0,9 -> 5,9", "8,0 -> 0,8", "123,456 -> 023,013"]

spec :: Spec
spec = do
  describe "Day 5" $ do
    it "parseInput" $ do
      parse parseInput "" inputText `shouldBe` Right [Line (0, 9) (5, 9), Line (8, 0) (0, 8), Line (123, 456) (23, 13)]

    it "getPoints" $ do
      getPoints (1, 2) (2, 3) `shouldBe` []
      getPoints (1, 2) (1, 2) `shouldBe` [(1, 2)]
      getPoints (1, 2) (1, 4) `shouldBe` [(1, 2), (1, 3), (1, 4)]
      getPoints (1, 4) (1, 2) `shouldBe` [(1, 2), (1, 3), (1, 4)]
      getPoints (5, 3) (3, 3) `shouldBe` [(3, 3), (4, 3), (5, 3)]
      getPoints (3, 3) (5, 3) `shouldBe` [(3, 3), (4, 3), (5, 3)]