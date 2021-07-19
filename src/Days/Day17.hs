module Days.Day17 where

import Common
import qualified Data.Set as S
import Algebra.Lattice
import Data.Monoid
import Data.Semigroup
import Data.Int
import Data.Strict.Tuple hiding (zip)
import Generic.Data
import GHC.Generics
import Data.Typeable

parser :: Integral a => Parser (Set (a,a,a,a))
parser = do
  raw <- many printChar `endBy` newline
  pure $ S.fromList [ (x,y,0,0) | (line, y) <- zip raw [0..], ('#',x) <- zip line [0..] ]

neighborhood :: Integral a => Bool -> (a,a,a,a) -> Set (a,a,a,a)
neighborhood p2 (x,y,z,w) = S.fromList $ (,,,) <$> [x-1..x+1] <*> [y-1..y+1] <*> [z-1..z+1] <*> ws
  where ws = if p2 then [w-1..w+1] else pure 0

localStep :: Bool -> Int -> Bool
localStep b = (== 4) /\ const b \/ (== 3)

step :: Integral a => Bool -> Endo (Set (a,a,a,a))
step p2 = Endo $ \s -> S.fromList $ do
  x <- [-6..13]
  y <- [-6..13]
  z <- [-6..13]
  w <- if p2 then [-6..6] else pure 0
  guard $ localStep (S.member (x,y,z,w) s) (S.size . (/\ s) . neighborhood p2 $ (x,y,z,w))
  pure (x,y,z,w)

main :: Text -> IO ()
main = withParser (parser @Int8) $ \input -> do
  print $ typeRep (undefined :: Proxy (Rep (Bool, Int)))
  print $ typeRep (undefined :: Proxy (Rep Cell))
  print . S.size . appEndo (stimes (6 :: Int) $ step False) $ input
  print . S.size . appEndo (stimes (6 :: Int) $ step True) $ input

newtype Cell = Cell (Bool, Int)
  deriving Semigroup via (Any, Sum Int)
  deriving Generic
