countIncreases :: Int -> [Int] -> Int
countIncreases count [] = count
countIncreases count [x] = count
countIncreases count (x:y:zx)
    | x < y = countIncreases (count + 1) (y:zx)
    | otherwise = countIncreases count (y:zx)

calc :: String -> Int
calc x = countIncreases 0 $ read <$> lines x

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ calc content