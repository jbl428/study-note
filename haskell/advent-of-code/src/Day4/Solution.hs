module Day4.Solution
  ( BingoResult (..),
    bingoCount,
    getResult,
    numberOfCountToBingo,
    solvePart1,
  )
where

import Control.Monad (guard)
import Data.List (elemIndex, minimumBy, transpose, (\\))
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (catMaybes, mapMaybe)
import Flow ((|>))
import Text.Parsec (ParseError, char, count, digit, many1, newline, parse, sepBy1, sepEndBy1, skipMany, spaces)
import Text.Parsec.String (Parser)

type Count = Int

type Score = Int

data BingoResult = Lose | Win Count Score
  deriving (Show, Eq)

type Board = [[Int]]

type Inputs = [Int]

numberOfCountToBingo :: Inputs -> [Int] -> Maybe Int
numberOfCountToBingo inputs row =
  do
    idx <- traverse (`elemIndex` inputs) row
    idxList <- nonEmpty idx
    return (maximum idxList + 1)

bingoCount :: Board -> Inputs -> Maybe Int
bingoCount board inputs =
  (board <> transpose board)
    |> fmap (numberOfCountToBingo inputs)
    |> catMaybes
    |> nonEmpty
    |> fmap minimum

getResult :: Inputs -> Board -> BingoResult
getResult inputs board =
  case bingoCount board inputs of
    Just count -> Win count (getScore count)
    _ -> Lose
  where
    getScore count =
      concat board \\ take count inputs
        |> sum
        |> (*) (inputs !! (count - 1))

numbers :: Parser Int
numbers = fmap read (many1 digit)

parseInputs :: Parser Inputs
parseInputs = sepBy1 numbers (char ',')

parseBoard :: Parser Board
parseBoard = count 5 row
  where
    row = count 5 (spaces *> numbers <* skipMany (char ' '))

parsePuzzleInput :: Parser (Inputs, [Board])
parsePuzzleInput = do
  inputs <- parseInputs
  newline
  boards <- sepEndBy1 parseBoard newline
  return (inputs, boards)

solvePart1 :: String -> Either ParseError Score
solvePart1 content = do
  (inputs, boards) <- parse parsePuzzleInput "" content
  let (_, score) =
        boards
          |> mapMaybe
            ( \board -> case getResult inputs board of
                (Win c s) -> Just (c, s)
                _ -> Nothing
            )
          |> minimumBy (\a b -> compare (fst a) (fst b))
  return score
