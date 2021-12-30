module Day3.SolutionSpec where

import Day3.Solution (Bit (..), getMostCommonBit, getRate, solve)
import Test.Hspec (Spec, describe, it, shouldBe)

exampleInput :: [String]
exampleInput =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  ]

spec :: Spec
spec = do
  describe "Day 3" $ do
    it "getMostCommonBit" $ do
      getMostCommonBit "10" `shouldBe` One
      getMostCommonBit "100" `shouldBe` Zero
      getMostCommonBit "011" `shouldBe` One
      getMostCommonBit "000" `shouldBe` Zero

    it "getRate" $ do
      getRate ["1100", "0101", "1001"] `shouldBe` (13, 2)
      getRate exampleInput `shouldBe` (22, 9)

    it "solve" $ do
      solve (unlines exampleInput) `shouldBe` 22 * 9
