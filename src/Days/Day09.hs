module Days.Day09 where

import Common
import Data.List
import Control.Applicative
import Control.Monad.State
import Control.Lens hiding (uncons)

main :: Text -> IO ()
main = withParser (decimal `endBy` newline) $ \input ->
  let results = forM_ (mapMaybe uncons $ tails (reverse input) >>= inits) $ \(x, xs) -> do
        when (length xs == 25 && and (liftA2 (((/= x) .) . (+)) xs xs)) $ _1 .= x
        _2 %= if sum xs == part1 && length xs >= 2 then const (minimum xs + maximum xs) else id
      (part1, part2) = execState results (0 :: Integer, 0)
  in print part1 >> print part2
