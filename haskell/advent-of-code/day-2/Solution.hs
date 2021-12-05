import Data.Maybe (mapMaybe)

data Mode = Forward | Up | Down deriving (Show)

toInputs :: [String] -> Maybe (Mode, Int)
toInputs [mode, amount] = case mode of
  "forward" -> Just (Forward, value)
  "up" -> Just (Up, value)
  "down" -> Just (Down, value)
  _ -> Nothing
  where
    value = read amount :: Int
toInputs _ = Nothing

calc :: [(Mode, Int)] -> (Int, Int)
calc = foldl temp (0, 0)
  where
    temp (h, d) (m, a) = case m of
      Forward -> (h + a, d)
      Up -> (h, d - a)
      Down -> (h, d + a)

solution :: String -> Int
solution = uncurry (*) . calc . mapMaybe (toInputs . words) . lines

main :: IO ()
main = readFile "input.txt" >>= print . solution