module Day4.Solution
  ( BingoResult (..),
    bingoCount,
    getResult,
    numberOfCountToBingo,
  )
where

import Data.List (elemIndex, transpose, (\\))
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (catMaybes)
import Flow ((|>))

type Count = Int

type Score = Int

data BingoResult = Lose | Win Count Score
  deriving (Show, Eq)

numberOfCountToBingo :: [Int] -> [Int] -> Maybe Int
numberOfCountToBingo inputs row =
  do
    idx <- traverse (`elemIndex` inputs) row
    idxList <- nonEmpty idx
    return (maximum idxList + 1)

bingoCount :: [[Int]] -> [Int] -> Maybe Int
bingoCount board inputs =
  (board <> transpose board)
    |> fmap (numberOfCountToBingo inputs)
    |> catMaybes
    |> nonEmpty
    |> fmap maximum

getResult :: [[Int]] -> [Int] -> BingoResult
getResult board inputs =
  case bingoCount board inputs of
    Just count -> Win count (getScore count)
    _ -> Lose
  where
    getScore count =
      concat board \\ take count inputs
        |> sum
        |> (*) (inputs !! (count - 1))