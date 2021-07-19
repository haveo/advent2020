module Days.Day03 where

import Common

parser :: Parser [[Char]]
parser = many printChar `endBy` newline

checkSlope :: Int -> Int -> Int -> [[Char]] -> Int
checkSlope k a b =
  length . filter (== '#') . fmap (\(i, row) -> row !! (a*(i `div` b) `mod` k)) . filter ((== 0) . (`mod` b). fst) . zip [0..]

main :: Text -> IO ()
main = withParser parser $ \input -> do
  let k = length (head input)
  print . product . fmap (\(a, b) -> checkSlope k a b input) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
