module Days.Day11 where

import Common
import qualified Data.Map as M

data Cell = Empty | Free | Occupied
  deriving (Eq, Show)

parseCell :: Parser Cell
parseCell = Empty <$ "." <|> Free <$ "L" <|> Occupied <$ "#"

parser :: Parser [[Cell]]
parser = many parseCell `endBy` newline

main = withParser parser $ \input ->
  let world = M.fromList $ do
        (i, cs) <- zip [0..] input
        (j, c) <- zip [0..] cs
        return ((i, j), c)
      square :: [(Integer, Integer)]
      square = filter (/= (0,0)) ((,) <$> [-1..1] <*> [-1..1])
      step cs c = case c of
                    Empty -> Empty
                    Free -> if Just Occupied `notElem` cs then Occupied else Free
                    Occupied -> if (length . filter (== Just Occupied) $ cs) >= 4 then Free else Occupied
      firstTowards :: _ -> _ -> _ -> Integer
      firstTowards m (i, j) (dx, dy) = case M.lookup (i+dx,j+dy) m of
                                         Nothing -> 0
                                         Just Empty -> firstTowards m (i+dx, j+dy) (dx, dy)
                                         Just Occupied -> 1
                                         Just Free -> 0
      next w = M.mapWithKey (\(i, j) c ->
        step ((w M.!?) . bimap (+i) (+j) <$> square) c) w
      nextTrans w = M.mapWithKey (\(i, j) ->
        \case
          Empty -> Empty
          Occupied -> if sum (firstTowards w (i, j) <$> square) >= 5 then Free else Occupied
          Free -> if sum (firstTowards w (i, j) <$> square) == 0 then Occupied else Free) w
      go f w = if n == w then w else go f n
        where n = f w
  in do
    print . M.size . M.filter (Occupied ==) $ go next world
    print . M.size . M.filter (Occupied ==) $ go nextTrans world
