module Days.Day22 where

import Common
import Data.Sequence as S
import qualified Data.Set as Set hiding (toList)
import Data.Foldable
import Data.Hashable

parser :: Num a => Parser (Seq a, Seq a)
parser = (,) <$> playerP <* newline <*> playerP
  where playerP = "Player " *> (decimal :: Parser Int) *> ":\n" *> (S.fromList <$> decimal `endBy` newline)

trick (x :<| xs) (y :<| ys) | x > y = Left (xs |> x |> y, ys)
trick (x :<| xs) (y :<| ys) | y > x = Left (xs, ys |> y |> x)
trick xs Empty = Right xs
trick Empty ys = Right ys

game :: Ord a => Seq a -> Seq a -> Seq a
game xs ys = case trick xs ys of
               Left (xs, ys) -> game xs ys
               Right zs -> zs

hash' :: (Hashable a1, Hashable a2, Foldable t1, Foldable t2) => (t1 a1, t2 a2) -> Int
hash' (xs, ys) = hash (toList xs, toList ys)

trick2 :: (Hashable a, Integral a) => Seq a -> Seq a -> Either (Seq a, Seq a) (Bool, Seq a)
trick2 (x :<| xs) (y :<| ys) | S.length xs >= fromIntegral x && S.length ys >= fromIntegral y =
  winTrick (fst $ game2 Set.empty (S.take (fromIntegral x) xs) (S.take (fromIntegral y) ys)) (x <| xs) (y <| ys)
trick2 xs@(x :<| _) ys@(y :<| _) | x > y = winTrick False xs ys
trick2 xs@(x :<| _) ys@(y :<| _) | y > x = winTrick True xs ys
trick2 xs Empty = Right (False, xs)
trick2 Empty ys = Right (True, ys)

winTrick :: Bool -> Seq a -> Seq a -> Either (Seq a, Seq a) b
winTrick False (x :<| xs) (y :<| ys) = Left (xs |> x |> y, ys)
winTrick True (x :<| xs) (y :<| ys) = Left (xs, ys |> y |> x)

game2 :: (Hashable a, Integral a) => Set Int -> Seq a -> Seq a -> (Bool, Seq a)
game2 seen xs ys | hash' (xs,ys) `Set.member` seen = (False, xs)
game2 seen xs ys = case trick2 xs ys of
                     Left (xs', ys') -> game2 (Set.insert (hash' (xs,ys)) seen) xs' ys'
                     Right r -> r

main :: Text -> IO ()
main = withParser parser $ \input -> do
   print . score . uncurry game $ input
   print . score . snd . uncurry (game2 Set.empty) $ input
     where
       score :: Seq Int -> Int
       score = sum . Prelude.zipWith (*) [1..] . Prelude.reverse . toList
