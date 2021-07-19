module Days.Day23 where

import Common hiding ((!))
import Data.IntMap.Strict as M
import Data.Monoid
import Data.Semigroup
import Data.Char

parser = many (digitToInt <$> printChar) <* newline <* eof

next !n (!i, !m) =
  (m' ! i, m')
  where
    t1 = m ! i
    t2 = m ! t1
    t3 = m ! t2
    normalize = (+1) . (`mod` n) . subtract 1
    target = fromJust . find (\x -> x `notElem` [t1,t2,t3]) . fmap normalize $ [(i-1),(i-2) ..]
    m' = M.insert i (m ! t3) . M.insert target t1 . M.insert t3 (m ! target) $ m

nmap xs = M.fromList $ zip xs (tail xs ++ [head xs])

main :: Text -> IO ()
main = withParser parser $ \input -> do
  let bigInput = input ++ [length input + 1 .. 1_000_000]
      crab k xs = appEndo (stimes k (Endo . next $ length xs)) (head xs, nmap xs)
      from i nm = next : from next nm
        where next = nm ! i
  putStrLn . fmap intToDigit . Prelude.take (length input - 1) . from 1 . snd . crab (100 :: Int) $ input
  print . (\(x:y:_) -> x*y) . from 1 . snd . crab (10_000_000 :: Int) $ bigInput
