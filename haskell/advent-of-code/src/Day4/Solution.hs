module Day4.Solution
  ( BingoResult (..),
    bingoCount,
    getResult,
    numberOfCountToBingo,
    parsePuzzleInput,
  )
where

import Control.Monad (guard)
import Data.List (elemIndex, transpose, (\\))
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (catMaybes)
import Flow ((|>))
import Text.Parsec (char, count, digit, many1, newline, sepBy1, sepEndBy1, skipMany, spaces)
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

getResult :: Board -> Inputs -> BingoResult
getResult board inputs =
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
