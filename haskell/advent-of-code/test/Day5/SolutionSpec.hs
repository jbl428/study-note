module Day5.SolutionSpec where

import Day5.Solution (Line (Line), getPoints, parseInput, solvePart1)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (parse)

inputText :: String
inputText = unlines ["0,9 -> 5,9", "8,0 -> 0,8", "123,456 -> 023,013"]

examples =
  unlines
    [ "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2"
    ]

spec :: Spec
spec = do
  describe "Day 5" $ do
    it "parseInput" $ do
      parse parseInput "" inputText `shouldBe` Right [Line (0, 9) (5, 9), Line (8, 0) (0, 8), Line (123, 456) (23, 13)]

    it "getPoints" $ do
      getPoints (Line (1, 2) (2, 3)) `shouldBe` []
      getPoints (Line (1, 2) (1, 2)) `shouldBe` [(1, 2)]
      getPoints (Line (1, 2) (1, 4)) `shouldBe` [(1, 2), (1, 3), (1, 4)]
      getPoints (Line (1, 4) (1, 2)) `shouldBe` [(1, 2), (1, 3), (1, 4)]
      getPoints (Line (5, 3) (3, 3)) `shouldBe` [(3, 3), (4, 3), (5, 3)]
      getPoints (Line (3, 3) (5, 3)) `shouldBe` [(3, 3), (4, 3), (5, 3)]

    it "solvePart1" $ do
      solvePart1 examples `shouldBe` Right 5