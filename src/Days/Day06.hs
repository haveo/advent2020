module Days.Day06 where

import Common
import Data.Set hiding (foldr)
import Control.Monad

parser :: Parser [[String]]
parser = (some letterChar `endBy` newline) `sepEndBy` newline

main :: Text -> IO ()
main = withParser parser $ \input -> do
  print $ sum (size . foldr insert empty . join <$> input)
  print $ sum (size . foldr1 intersection . fmap fromList <$> input)
