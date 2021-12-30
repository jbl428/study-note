module Day3.Solution (solve, getMostCommonBit, Bit (..), getRate) where

import Data.List (transpose)

data Bit = Zero | One deriving (Show, Eq)

getMostCommonBit :: String -> Bit
getMostCommonBit xs =
  if length xs < zeroCount * 2
    then Zero
    else One
  where
    zeroCount = length . filter (== '0') $ xs

type Gamma = Int

type Epsilon = Int

getRate :: [String] -> (Gamma, Epsilon)
getRate xs = go digitList (2 ^ (length digitList -1)) (0, 0)
  where
    digitList = transpose xs
    go [] _ rates = rates
    go (str : strs) multiple (g, e) =
      if getMostCommonBit str == Zero
        then go strs (multiple `div` 2) (g, e + multiple)
        else go strs (multiple `div` 2) (g + multiple, e)

solve :: String -> Int
solve = uncurry (*) . getRate . lines