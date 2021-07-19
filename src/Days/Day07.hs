module Days.Day07 where

import Common
import qualified Data.Map as M
import Data.Semigroup
import Data.Functor.Compose
import Control.Lens

data Repeat n a = Repeat n a

instance Integral n => Foldable (Repeat n) where
  foldMap f (Repeat n x) = stimes n (f x)

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

parser :: Integral n => Parser (Text, Compose [] (Repeat n) Text)
parser = (,) <$> (pack <$> manyTill printChar " bags contain ") <*> fmap Compose content <* "."
  where
    content = [] <$ "no other bags" <|> bagsInside `sepBy` ", "
    bagsInside = Repeat <$> decimal <* " " <*> (pack <$> manyTill printChar (" bag" <* optional "s"))

main :: Text -> IO ()
main = withParser (M.fromList <$> (parser @Integer `endBy` newline)) $ \input -> do
  let foldBags :: Monoid m => (Text -> m) -> Map Text m
      foldBags f = loeb (fmap (\fs r -> foldMap (\s -> f s <> r ! s) fs) input)
  print . M.size . M.filter id $ alaf Any foldBags (== "shiny gold")
  print . (! "shiny gold") $ alaf Sum foldBags (const (1 :: Int))
