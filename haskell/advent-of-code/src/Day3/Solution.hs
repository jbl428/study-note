module Day3.Solution
  ( solvePart1,
    getMostCommonBit,
    Bit (..),
    getRate,
    getOxygenBit,
    getCO2Bit,
    getOxygenRate,
    getCO2Rate,
    binaryToDecimal,
    solvePart2,
  )
where

import Data.List (transpose)

data Bit = Zero | One deriving (Show, Eq)

zeroCount :: String -> Int
zeroCount = length . filter (== '0')

getMostCommonBit :: String -> Bit
getMostCommonBit xs =
  if length xs < zeroCount xs * 2
    then Zero
    else One

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

solvePart1 :: String -> Int
solvePart1 = uncurry (*) . getRate . lines

getOxygenBit :: String -> Char
getOxygenBit xs =
  if length xs < zeroCount xs * 2
    then '0'
    else '1'

getCO2Bit :: String -> Char
getCO2Bit xs =
  if length xs < zeroCount xs * 2
    then '1'
    else '0'

getRateBy :: (String -> Char) -> [String] -> String
getRateBy bitFn strs = go strs 0
  where
    go [] _ = ""
    go [x] _ = x
    go xs idx = go temp (idx + 1)
      where
        bit = bitFn $ fmap (!! idx) xs
        temp = filter (\a -> a !! idx == bit) xs

getOxygenRate :: [String] -> String
getOxygenRate = getRateBy getOxygenBit

getCO2Rate :: [String] -> String
getCO2Rate = getRateBy getCO2Bit

binaryToDecimal :: String -> Int
binaryToDecimal b = go (reverse b) 1
  where
    go [] _ = 0
    go (x : xs) num =
      ( if x == '1'
          then num
          else 0
      )
        + go xs num * 2

solvePart2 :: String -> Int
solvePart2 xs = oxygen * co2
  where
    input = lines xs
    oxygen = binaryToDecimal $ getOxygenRate input
    co2 = binaryToDecimal $ getCO2Rate input