module Days.Day10 where

import Common
import Data.List (sort)
import Control.Lens hiding ((:<))
import Control.Comonad hiding (cfix)
import GHC.Exts (fromList)
import Data.Stream.Future
import Data.Foldable

cfix :: Future (Future a -> a) -> Future a
cfix (Last f) = fix (Last . f)
cfix (f :< fs) = fix ((:< cfix fs) . f)

main :: Text -> IO ()
main = withParser (fromList . (0:) . sort <$> decimal `endBy` newline) $ \input -> do
  print . (\xs -> howMany (== 3) xs * howMany (== 1) xs) . toList $ difference <<= input
  print . view head1 . cfix $ (\xs r -> sumNext $ (,) <$> xs <@> r) <<= input
 where
   sumNext (Last _) = 1 :: Integer
   sumNext ((x, _) :< fs) = foldr (\(y, ry) r -> if y - x > 3 then 0 else ry + r) 0 fs
   difference (Last _) = 3 :: Integer
   difference (x :< xs) = extract xs - x
