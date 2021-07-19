module Days.Day15 where

import Common
import qualified Data.HashMap.Strict as M
import Data.Array.MArray
import Control.Monad.ST
import Data.Array.ST
import Data.Word

parser :: Num a => Parser [a]
parser = decimal `sepBy` ","

loop :: (Eq a, Num a, MArray arr a m, Monad m, Ix a) => a -> a -> arr a a -> a -> m a
loop k !prev arr !i =
  if i == (k-1) then pure prev
  else do
    v <- readArray arr prev
    writeArray arr prev (i+1)
    loop k (if v == 0 then 0 else i - (v-1)) arr (i+1)

main :: Text -> IO ()
main = withParser parser $ \input ->
  let n = fromInteger . toInteger $ length input
      getKth k = runST $ do
        (arr :: STUArray s _ _) <- newArray (0, k) 0
        forM_ (zip input [1..]) $ uncurry (writeArray arr)
        (loop k 0 arr n :: ST s _)
  in do
    print @Word32 $ getKth 2020
    print @Word32 $ getKth 30000000
