module Day4.Solution
  ( BingoResult (..),
    getResult,
    numberOfCountToBingo,
  )
where

import Data.List (elemIndex)
import Data.List.NonEmpty (nonEmpty, toList)

type Count = Int

type Score = Int

data BingoResult = Lose | Win Count Score
  deriving (Show, Eq)

getResult :: [[Int]] -> [Int] -> BingoResult
getResult _ _ = Lose

numberOfCountToBingo :: [Int] -> [Int] -> Maybe Int
numberOfCountToBingo row inputs =
  do
    idx <- traverse (`elemIndex` inputs) row
    idxList <- nonEmpty idx
    return (sum idxList + 1)
