module Day5.Solution
  ( Line (..),
    parseInput,
  )
where

import Text.Parsec (char, digit, many1, newline, parseTest, sepEndBy1, string)
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
