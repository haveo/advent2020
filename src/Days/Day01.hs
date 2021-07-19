module Days.Day01 where

import Common

parser :: Parser [Integer]
parser = decimal `endBy` newline

main :: Text -> IO ()
main = withParser parser $ \input -> do
  let part1 = head $
        do
          a <- input
          b <- input
          guard (a + b == 2020)
          return (a*b)
      part2 = head $
        do
          a <- input
          b <- input
          c <- input
          guard (a + b + c == 2020)
          return (a*b*c)
  print part1
  print part2
