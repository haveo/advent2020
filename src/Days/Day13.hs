module Days.Day13 where

import Common

parser :: Num a => Parser (a, [Maybe a])
parser = (,) <$>
  decimal <* newline <*>
  (("x" $> Nothing <|> Just <$> decimal) `sepBy` ",") <* newline

main :: Text -> IO ()
main = withParser parser $ \(n, xs) ->
  let aux (i, n) (a, m) = (, lcm n m ) . fromJust . find (\x -> x+i `mod` n == 0) $ [a,a+m..]
  in do
    print @Integer . uncurry (*) . minimum . (`zip` catMaybes xs) . fmap ((-n) `mod`) . catMaybes $ xs
    print @Integer . fst . foldr aux (0, 1) . mapMaybe sequence . zip [0..] $ xs
