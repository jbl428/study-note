import Data.Maybe (mapMaybe)

data Mode = Forward | Up | Down deriving (Show)

toInputs :: [String] -> Maybe (Mode, Int)
toInputs [mode, unit] = case mode of
  "forward" -> Just (Forward, value)
  "up" -> Just (Up, value)
  "down" -> Just (Down, value)
  _ -> Nothing
  where
    value = read unit :: Int
toInputs _ = Nothing

calc :: [(Mode, Int)] -> (Int, Int, Int)
calc = foldl temp (0, 0, 0)
  where
    temp (horizontal, depth, aim) (mode, unit) = case mode of
      Forward -> (horizontal + unit, depth + aim * unit, aim)
      Up -> (horizontal, depth, aim - unit)
      Down -> (horizontal, depth, aim + unit)

solution :: String -> Int
solution = (\(a, b, _) -> a * b) . calc . mapMaybe (toInputs . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solution