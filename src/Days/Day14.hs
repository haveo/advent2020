module Days.Day14 where

import Common
import qualified Data.Map as M
import Data.Word
import Data.Foldable

parseMask = do
  xs <- reverse <$> count 36 (oneOf @[] "01X")
  let (m1, m0) = both (mkBinary xs) ('1', '0')
      mxs :: [Integer]
      mxs = mkBinaries xs 'X'
  pure ((m1,m0), \n -> fmap (\x -> (n .&. m0) .|. (complement 0 .&. m1) .|. x) mxs)
 where
  mkBinary [] _ = 0
  mkBinary (x:xs) c = (if x == c then 1 else 0) + 2 * mkBinary xs c
  mkBinaries [] _ = pure 0
  mkBinaries (x:xs) c = do
    b <- if x == c then [0,1] else pure 0
    bs <- mkBinaries xs c
    pure $ b + 2* bs

parser = (("mask = " *> parseMask) `eitherP` ((,) <$> ("mem[" *> decimal <* "] = ") <*> decimal)) `endBy` newline

eval (_, mem) (Left (m,_)) = (m, mem)
eval ((m1, m0), mem) (Right (addr, v)) = ((m1, m0) ,) $ M.insert addr ((v .|. m1) .&. complement m0) mem

eval2 (_, mem) (Left (_, f)) = (f, mem)
eval2 (f, mem) (Right (addr, v)) = (f,) $ foldr (`M.insert` v) mem (f addr)

main :: Text -> IO ()
main = withParser parser $ \input -> do
  print @Integer . sum . snd $ foldl eval ((0, 0), M.empty) input
  print @Integer . sum . snd $ foldl eval2 (const mempty, M.empty) input
