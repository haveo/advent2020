module Days.Day25 where

import Common

parser :: Integral a => Parser (a,a)
parser = (,) <$> (decimal <* newline) <*> decimal

loop :: Integral a => a -> a -> a
loop n i = (n*i) `mod` 20201227

solve :: (Integral a, Integral b) => a -> b -> [b]
solve i j = do
  (x,rx) <- zip [0..] (iterate (loop 7) 1)
  guard $ rx == i
  pure (iterate (loop j) 1 !! x)

main :: Text -> IO ()
main = withParser (parser @Int) $ \(card, door) -> do
  let rs = iterate (loop 7) 1
  print . head $ solve card door
