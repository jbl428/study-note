{-# LANGUAGE TupleSections #-}

module Day5.Solution
  ( Line (..),
    parseInput,
    getPoints,
    solvePart1,
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

solvePart1 :: String -> Either ParseError Int
solvePart1 content =
  do
    lines <- parse parseInput "" content
    return (lines >>= getPoints |> frequency |> filter (\(_, count) -> count > 1) |> length)

frequency :: [Coordinate] -> [(Coordinate, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])