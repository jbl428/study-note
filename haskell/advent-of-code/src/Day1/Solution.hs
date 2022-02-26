module Day1.Solution (solve) where

import Flow ((|>))

countIncreases :: Int -> [Int] -> Int
countIncreases count (x : y : zx)
  | x < y = countIncreases (count + 1) (y : zx)
  | otherwise = countIncreases count (y : zx)
countIncreases count _ = count

threeWindowsSum :: [Int] -> [Int]
threeWindowsSum (x : y : z : zs) = (x + y + z) : threeWindowsSum (y : z : zs)
threeWindowsSum _ = []

solve :: String -> Int
solve x =
  x
    |> lines
    |> fmap read
    |> threeWindowsSum
    |> countIncreases 0
