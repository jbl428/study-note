module Day1.Solution (solve) where

countIncreases :: Int -> [Int] -> Int
countIncreases count (x : y : zx)
  | x < y = countIncreases (count + 1) (y : zx)
  | otherwise = countIncreases count (y : zx)
countIncreases count _ = count

solve :: String -> Int
solve x = countIncreases 0 $ threeWindowsSum $ read <$> lines x

threeWindowsSum :: [Int] -> [Int]
threeWindowsSum (x : y : z : zs) = (x + y + z) : threeWindowsSum (y : z : zs)
threeWindowsSum _ = []
