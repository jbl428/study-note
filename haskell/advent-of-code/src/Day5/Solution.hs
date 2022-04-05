{-# LANGUAGE TupleSections #-}

module Day5.Solution
  ( Line (..),
    parseInput,
    getPoints,
    getPointsWithDiagonal,
    solvePart1,
    solvePart2,
  )
where

import Data.Map (fromListWith, toList)
import Flow ((|>))
import Text.Parsec (ParseError, char, digit, many1, newline, parse, parseTest, sepEndBy1, string)
import Text.Parsec.String (Parser)

type Coordinate = (Int, Int)

data Line = Line {from :: Coordinate, to :: Coordinate} deriving (Show, Eq)

parseInput :: Parser [Line]
parseInput = do
  sepEndBy1 line newline
  where
    numbers = fmap read (many1 digit)
    line = do
      x1 <- numbers
      char ','
      y1 <- numbers
      string " -> "
      x2 <- numbers
      char ','
      y2 <- numbers
      return $ Line (x1, y1) (x2, y2)

getPoints :: Line -> [Coordinate]
getPoints (Line (x1, y1) (x2, y2))
  | x1 == x2 = fmap (x1,) [(min y1 y2) .. (max y1 y2)]
  | y1 == y2 = fmap (,y1) [(min x1 x2) .. (max x1 x2)]
  | otherwise = []

frequency :: [Coordinate] -> [(Coordinate, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

isDiagonal :: Line -> Bool
isDiagonal (Line (x1, y1) (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

getPointsWithDiagonal :: Line -> [Coordinate]
getPointsWithDiagonal line@(Line (x1, y1) (x2, y2))
  | x1 == x2 = zip (repeat x1) (item y1 y2)
  | y1 == y2 = zip (item x1 x2) (repeat y1)
  | isDiagonal line = zip (item x1 x2) (item y1 y2)
  | otherwise = []
  where
    item a b = if a < b then [a .. b] else reverse [b .. a]

solveBy :: (Line -> [Coordinate]) -> String -> Either ParseError Int
solveBy pointFn content =
  do
    lines <- parse parseInput "" content
    return (lines >>= pointFn |> frequency |> filter (\(_, count) -> count > 1) |> length)

solvePart1 :: String -> Either ParseError Int
solvePart1 = solveBy getPoints

solvePart2 :: String -> Either ParseError Int
solvePart2 = solveBy getPointsWithDiagonal