module Day3.SolutionSpec where

import Day3.Solution
  ( Bit (..),
    binaryToDecimal,
    getCO2Bit,
    getCO2Rate,
    getMostCommonBit,
    getOxygenBit,
    getOxygenRate,
    getRate,
    solvePart1,
    solvePart2,
  )
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

    it "solvePart1" $ do
      solvePart1 (unlines exampleInput) `shouldBe` 22 * 9

    it "getOxygenBit" $ do
      getOxygenBit "1100" `shouldBe` '1'
      getOxygenBit "1110" `shouldBe` '1'
      getOxygenBit "1000" `shouldBe` '0'

    it "getCO2Bit " $ do
      getCO2Bit "1100" `shouldBe` '0'
      getCO2Bit "1110" `shouldBe` '0'
      getCO2Bit "1000" `shouldBe` '1'

    it "getOxygenRate" $ do
      getOxygenRate exampleInput `shouldBe` "10111"

    it "getCO2Rate" $ do
      getCO2Rate exampleInput `shouldBe` "01010"

    it "binaryToDecimal" $ do
      binaryToDecimal "0" `shouldBe` 0
      binaryToDecimal "01" `shouldBe` 1
      binaryToDecimal "101" `shouldBe` 5
      binaryToDecimal "110" `shouldBe` 6
      binaryToDecimal "1001" `shouldBe` 9

    it "solvePart2" $ do
      solvePart2 (unlines exampleInput) `shouldBe` 230