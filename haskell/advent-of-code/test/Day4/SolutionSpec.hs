module Day4.SolutionSpec where

import Day4.Solution (BingoResult (..), getResult, numberOfCountToBingo)
import Flow ((|>))
import Test.Hspec (Spec, describe, it, shouldBe)

numbers :: [Int]
numbers = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]

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

spec :: Spec
spec = do
  describe "Day 4" $ do
    it "numberOfCountToBingo" $ do
      numberOfCountToBingo [] [1, 2] `shouldBe` Nothing
      numberOfCountToBingo [1, 2] [] `shouldBe` Nothing
      numberOfCountToBingo [1, 2] [3, 4] `shouldBe` Nothing
      numberOfCountToBingo [1, 2] [1, 2] `shouldBe` Just 2
      numberOfCountToBingo [1, 2] [1, 3, 1, 2, 2] `shouldBe` Just 4
    it "getResult" $ do
      getResult boardA [] `shouldBe` Lose

-- getResult boardA [22, 13, 17, 11, 0] `shouldBe` Win 5 (boardA |> tail |> concat |> sum)