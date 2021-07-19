module Days.Day24 where

import Common
import Algebra.Lattice
import Data.Monoid
import Data.Semigroup
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

newtype Coord a = Coord (a, a)
 deriving (Semigroup, Monoid) via (Sum a, Sum a)
 deriving (Show, Ord, Eq)

parser :: Integral a => Parser [[Coord a]]
parser = many direction `endBy` newline
  where direction = choice
          [ "e" $>  Coord (1,0)
          , "w" $>  Coord (-1,0)
          , "ne" $> Coord (0,1)
          , "nw" $> Coord (-1,1)
          , "se" $> Coord (1,-1)
          , "sw" $> Coord (0,-1)
          ]

localStep :: (Eq a, Num a) => Bool -> a -> Bool
localStep b = (== 2) \/ bool (const False) (== 3) b

neighborhood :: Integral a => Coord a -> [Coord a]
neighborhood c = do
  y <- [-1..1]
  x <- [max (-1) (-(1+y)) .. min 1 (1-y)]
  pure $ c <> Coord (x,y)

step :: forall a. Integral a => Endo (Set (Coord a))
step = Endo $ \s ->
  let neighbours :: Map (Coord a) Int
      neighbours = M.fromListWith (+) . fmap (, 1) .  (>>= neighborhood) . toList $ s
  in M.keysSet . M.filterWithKey (\c -> localStep (S.member c s)) $ neighbours

main :: Text -> IO ()
main = withParser (parser @Int) $ \input -> do
  let init = M.keysSet . M.filter ((== 1) . (`mod` 2)) . M.fromListWith (+) . fmap ((, 1 :: Int) . fold) $ input
  print . S.size $ init
  print . S.size . appEndo (stimes (100 :: Int) step) $ init
