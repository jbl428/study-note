countIncreases :: Int -> [Int] -> Int
countIncreases count (x : y : zx)
  | x < y = countIncreases (count + 1) (y : zx)
  | otherwise = countIncreases count (y : zx)
countIncreases count _ = count

calc :: String -> Int
calc x = countIncreases 0 $ threeWindowsSum $ read <$> lines x

threeWindowsSum :: [Int] -> [Int]
threeWindowsSum (x : y : z : zs) = (x + y + z) : threeWindowsSum (y : z : zs)
threeWindowsSum _ = []

main :: IO ()
main = readFile "input.txt" >>= print . calc