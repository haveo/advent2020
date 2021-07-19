module Days.Day21 where

import Common
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Char
import Data.List

parser = foodP `endBy` newline
  where
    wordP = takeWhile1P Nothing isLetter
    foodP = do
      ingredients <- wordP `sepEndBy` " "
      _ <- "(contains "
      allergens <- wordP `sepBy` ", "
      _ <- ")"
      pure (allergens , S.fromList ingredients)

main :: Text -> IO ()
main = withParser parser $ \input ->
  let m :: Map Text (Set Text)
      m = M.fromListWith S.intersection $ do
            (is, als)  <- input
            i <- is
            pure (i, als)
      m' = simplify m
      ingredients = foldl1 S.union . fmap snd $ input
      unsafe = S.fromList . M.elems $ m'
      safe = S.difference ingredients unsafe
   in do
     print . sum . fmap (S.size . S.filter (`S.member` safe) . snd) $ input
     T.putStrLn $ T.intercalate "," . fmap snd . sortOn fst . M.toList $ m'


simplify :: Ord b => Map a (Set b) -> Map a b
simplify m =
  if length solved == M.size m then fmap (fromJust . fromSingleton) m
  else simplify $ fmap (\s -> case fromSingleton s of
                     Nothing -> S.difference s solved
                     Just _ -> s) m
  where
    solved = S.fromList $ mapMaybe fromSingleton (M.elems m)
    fromSingleton s = case S.toList s of
                        [x] -> Just x
                        _ -> Nothing

