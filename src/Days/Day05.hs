module Days.Day05 where

import Common
import Data.List

parser :: Parser [Int]
parser = (foldl (\n x -> 2*n+(ord x `mod` 7) `mod` 2) 0 <$> many letterChar) `endBy` newline

main :: Text -> IO ()
main = withParser parser $ \input -> do
  print $ foldr max 0 input
  print . head $ [minimum input..] \\ input
