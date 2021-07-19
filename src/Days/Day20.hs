module Days.Day20 where

import Common
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Monoid
import Data.Semigroup
import Data.List
import Data.Foldable
import Data.Bifunctor
import Data.Tuple

parser :: Parser (Map Int [[Char]])
parser = fmap M.fromList . many $ do
  i <- "Tile " *> decimal <* ":\n"
  grid <- many (some printChar <* newline) <* newline
  pure (i, grid)

rot :: [[a]] -> [[a]]
rot = transpose . reverse

exHead :: [a] -> a
exHead [x] = x

main :: Text -> IO ()
main = withParser (parser <* eof) $ \input -> do
  let n = floor . (sqrt @Float) . fromIntegral . M.size $ input
      m = length . head . snd . M.findMin $ input
      graph = M.mapWithKey (\i g -> filter (/= i) . M.keys . M.mapMaybe (match g) $ input) input
      pieces = M.toList input
      gen (x,0) = exHead $ do
        let (i, left) = puzzle ! (x-1,0)
        (j, candidate) <- pieces
        guard (i /= j)
        res <- matchRight left candidate
        pure (j, res)
      gen (x,y) = exHead $ do
        let (i, top) = puzzle ! (x,y-1)
        (j, candidate) <- pieces
        guard (i /= j)
        res <- matchBottom top candidate
        pure (j, res)
      coords = (,) <$> [0..n-1] <*> [0..n-1]
      corner = (\i -> (i, input ! i)) . head . M.keys . M.filter ((== 2) . length) $ graph
      puzzle = M.insert (0, 0) corner $ M.fromList . fmap (\x -> (x,gen x)) $  coords
      f xss = dropFirstLast (fmap dropFirstLast xss)
        where dropFirstLast xs = drop 1 $ take (length xs - 1) xs
      mkGrid (x, y) zss = do
        (j, zs) <- zip [0..] (f zss)
        (i, z) <- zip [0..] zs
        pure ((i+x*8, j+y*8), z)
      grid = M.fromList . (>>= uncurry mkGrid) . (M.toList . fmap snd) $ puzzle
  --print $ M.keysSet grid == S.fromList ((,) <$> [0..95] <*> [0..95])
  print . M.keys . M.filter ((== 2) . length) $ graph
  print . M.size $ grid
  forM_ symmetries $ \sym ->
    let x = foldl S.union mempty . findMonsters $ M.mapKeys sym grid
        total = M.size $ M.filter (== '#') grid
    in when (x /= S.empty) $ do
      putStrLn "found:"
      print (total - S.size x)
  print . subtract 15 . M.size . M.filter (== '#') $ grid

transformations :: [Endo [[a]]]
transformations = fmap fold . traverse (: pure mempty) $ Endo <$> [ reverse, fmap reverse, transpose ]

rotations = iterate (. rot) id

matchBottom g1 g2 = (\l -> assert (length l <= 1) l) $ do
  r <- [0..3]
  mirror <- [False, True]
  guard $ head (reverse g1) == head ((rotations !! r) . bool id transpose mirror $ g2)
  pure ((rotations !! r) . bool id transpose mirror $ g2)

matchRight g1 g2 = (\l -> assert (length l <= 1) l) $ do
  r <- [0..3]
  mirror <- [False, True]
  guard $ head (transpose . fmap reverse . reverse $ g1) == head ((rotations !! r) . bool id transpose mirror $ g2)
  pure ((rotations !! (r+3)) . bool id transpose mirror $ g2)

match g1 g2 = listToMaybe $ do
  r1 <- [0..3]
  r2 <- [0..3]
  mirror <- [False, True]
  guard $ head ((rotations !! r1) . bool id transpose mirror $ g1) == head ((rotations !! r2) g2)
  pure (r1, r2, mirror)

symmetries :: Integral a => [(a,a) -> (a,a)]
symmetries = do
  sx <- [-1,1]
  sy <- [-1,1]
  t <- [swap,id]
  pure $ \(x,y) -> t ((sx*x) `mod` 96,(sy*y) `mod` 96)

pattern :: [(Int, Int)]
pattern = mapMaybe (\(k, c) -> if c == '#' then Just (k `div` 20,k `mod` 20) else Nothing) . zip [0..] $ s
  where s = "                  # #    ##    ##    ### #  #  #  #  #  #   "

translate (x,y) (i,j) = (x+i,y+j)

matchMonster :: Integral a => [(a,a)] -> Map (a,a) Char -> [Set (a,a)]
matchMonster monster grid =
  if all (\(x,y) -> grid M.!? (x,y) == Just '#') $ monster
  then pure (S.fromList monster)
  else []

findMonsters grid = do
  x <- [0..100]
  y <- [0..117]
  let monster = translate (x,y) <$> pattern
  matchMonster monster grid

test = M.fromList $ do
  (i, xs) <- zip [0..] raw
  (j, x) <- zip [0..] xs
  pure ((i, j), x)
  where raw = [ ".#.#..#.##...#.##..#####"
              , "###....#.#....#..#......"
              , "##.##.###.#.#..######..."
              , "###.#####...#.#####.#..#"
              , "##.#....#.##.####...#.##"
              , "...########.#....#####.#"
              , "....#..#...##..#.#.###.."
              , ".####...#..#.....#......"
              , "#..#.##..#..###.#.##...."
              , "#.####..#.####.#.#.###.."
              , "###.#.#...#.######.#..##"
              , "#.####....##..########.#"
              , "##..##.#...#...#.#.#.#.."
              , "...#..#..#.#.##..###.###"
              , ".#.#....#.##.#...###.##."
              , "###.#...#..#.##.######.."
              , ".#.#.###.##.##.#..#.##.."
              , ".####.###.#...###.#..#.#"
              , "..#.#..#..#.#.#.####.###"
              , "#..####...#.#.#.###.###."
              , "#####..#####...###....##"
              , "#.##..#..#...#..####...#"
              , ".#.###..##..##..####.##."
              , "...###...##...#...#..###"
              ]
