module Day4.SolutionSpec where

import Day4.Solution (BingoResult (..), bingoCount, getResult, numberOfCountToBingo, solvePart1, solvePart2)
import Flow ((|>))
import Test.Hspec (Spec, describe, it, shouldBe)

inputs :: [Int]
inputs = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

boardA :: [[Int]]
boardA =
  [ [22, 13, 17, 11, 0],
    [8, 2, 23, 4, 24],
    [21, 9, 14, 16, 7],
    [6, 10, 3, 18, 5],
    [1, 12, 20, 15, 19]
  ]

boardB :: [[Int]]
boardB =
  [ [3, 15, 0, 2, 22],
    [9, 18, 13, 17, 5],
    [19, 8, 7, 25, 23],
    [20, 11, 10, 24, 4],
    [14, 21, 16, 12, 6]
  ]

boardC :: [[Int]]
boardC =
  [ [14, 21, 17, 24, 4],
    [10, 16, 15, 9, 19],
    [18, 8, 23, 26, 20],
    [22, 11, 13, 6, 5],
    [2, 0, 12, 3, 7]
  ]

inputText =
  unlines
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
      "",
      "22 13 17 11  0",
      " 8  2 23  4 24",
      "21  9 14 16  7",
      " 6 10  3 18  5",
      " 1 12 20 15 19",
      "",
      " 3 15  0  2 22",
      " 9 18 13 17  5",
      "19  8  7 25 23",
      "20 11 10 24  4",
      "14 21 16 12  6",
      "",
      "14 21 17 24  4",
      "10 16 15  9 19",
      "18  8 23 26 20",
      "22 11 13  6  5",
      " 2  0 12  3  7"
    ]

spec :: Spec
spec = do
  describe "Day 4" $ do
    it "numberOfCountToBingo" $ do
      numberOfCountToBingo [] [1, 2] `shouldBe` Nothing
      numberOfCountToBingo [1, 2] [] `shouldBe` Nothing
      numberOfCountToBingo [1, 2] [3, 4] `shouldBe` Nothing
      numberOfCountToBingo [1, 2] [1, 2] `shouldBe` Just 2
      numberOfCountToBingo [1, 3, 0, 1, 2, 2] [1, 2, 3] `shouldBe` Just 5

    it "bingoCount" $ do
      bingoCount boardA [] `shouldBe` Nothing
      bingoCount boardA [22, 13, 17, 0, 11] `shouldBe` Just 5
      bingoCount boardB [22, 13, 17, 5, 23, 4, 1, 5, 6, 10] `shouldBe` Just 9
      bingoCount boardC inputs `shouldBe` Just 12

    it "getResult" $ do
      getResult [] boardA `shouldBe` Lose
      getResult [22, 13, 17, 0, 11] boardA `shouldBe` Win 5 (boardA |> tail |> concat |> sum |> (* 11))
      getResult inputs boardC `shouldBe` Win 12 (188 * 24)

    it "solvePart1" $ do
      solvePart1 inputText `shouldBe` Right 4512

    it "solvePart2" $ do
      solvePart2 inputText `shouldBe` Right 1924
