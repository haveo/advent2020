module Days.Day02 where

import Common
import Data.Maybe

data Policy = Policy
  { a :: Integer
  , b :: Integer
  , char :: Char
  } deriving Show

(&&&) p q x = p x && q x

parseLine :: Parser (Policy, Text)
parseLine = do
  a <- decimal
  _ <- chunk "-"
  b <- decimal
  _ <- chunk " "
  char <- lowerChar
  _ <- chunk ": "
  s <- takeWhileP Nothing (/= '\n')
  return (Policy {..}, s)

parser :: Parser [(Policy, Text)]
parser = parseLine `endBy` newline

validateFirst :: Policy -> Text -> Bool
validateFirst (Policy min max c) =
  ((>= min) &&& (<= max)) . toInteger . length . filter (== c) .  unpack

validateSecond :: Policy -> Text -> Bool
validateSecond (Policy i j c) (unpack -> s) = fromMaybe False $ do
  x <- getIndex i
  y <- getIndex j
  return $ (x == c) /= (y == c)
    where getIndex (fromInteger -> k) = do
            guard (k <= length s)
            return $ s !! (k- 1)

main :: Text -> IO ()
main = withParser parser $ \input -> do
  print $ length $ filter (uncurry validateFirst) input
  print $ length $ filter (uncurry validateSecond) input
