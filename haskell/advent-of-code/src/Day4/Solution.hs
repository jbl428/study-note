module Day4.Solution
  ( BingoResult (..),
    getResult,
  )
where

type Count = Int

type Score = Int

data BingoResult = Lose | Win Count Score
  deriving (Show, Eq)

getResult :: [[Int]] -> [Int] -> BingoResult
getResult _ _ = Lose
