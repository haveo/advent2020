module Days.Day12 where

import Common
import Data.Foldable
import Data.Monoid
import Data.Semigroup
import Control.Lens

newtype Coord a = Coord (a, a)
 deriving (Semigroup, Monoid) via (Sum a, Sum a)

parser :: (Monoid a, Monoid b) => [a] -> b -> Parser (ASetter' (a -> a, b) a -> (a -> a, b))
parser directions rot = mdo
  result <- choice $ (\(p, f) -> p $> (flip $ f n) mempty) <$>
    [("F", forward)]
    ++ zip ["R", "L"] (turn <$> ([1, -1] :: [Integer]))
    ++ zip ["E", "S", "W", "N"] (move <$> directions)
  n <- decimal
  pure result
 where
  forward n _ = set _1 $ stimes n
  turn k  n _ = set _2 $ stimes (k * n `mod` 360 `div` 90) rot
  move v  n l = over l (<> stimes n v)

main :: Text -> IO ()
main = withParser (parser directions rotate `endBy` newline) $ \input ->
   forM_ [(Coord (1,0), _1 . mapped), (Coord (10, 1), _2 .  _Wrapped . mapped)] $ \(v0, l) ->
     let (shipMoves, vectorTransforms) = unzip $ sequence input l
         vectors = scanl (flip appEndo) v0 vectorTransforms
     in print @Integer . (\(Coord (x, y)) -> abs x + abs y) . fold $ zipWith id shipMoves vectors
 where
   rotate = Endo $ \(Coord (x, y)) -> Coord (y, -x)
   directions = iterate (appEndo rotate) $ Coord (1, 0)
