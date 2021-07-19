module Days.Day19 where

import Common
import Data.Map as M
import Control.Monad.Fix

rulesP :: Integral a => Map a (Parser ()) -> Parser (Map a (Parser ()))
rulesP res =
  M.fromList <$> (rule `endBy` newline)
  where
    rule = do
      i <- decimal
      _ <- ": "
      p <- case i of
             0 -> takeWhileP Nothing (/= '\n') $> parse0
             _ -> simple <|> complex
      pure (i, p)
    parse0 = do
      xs <- some (try (res ! 42))
      ys <- some (try (res ! 31))
      guard $ xs > ys
      pure ()
    simple :: Parser (Parser ())
    simple = "\"" *> fmap (\c -> char c $> ()) printChar <* "\""
    complex :: Parser (Parser ())
    complex = do
      p1 <- conc
      alt p1 <|> pure p1
    conc :: Parser (Parser ())
    conc = foldl1 (*>) <$> (((res !) <$> decimal) `sepEndBy` " ")
    alt :: Parser () -> Parser (Parser ())
    alt p1 = "| " *> ((\p2 -> try p1 <|> p2) <$> conc)

parser = do
  ((! (0 :: Int)) -> rule0P) <- mfix rulesP
  _ <- newline
  ((try (rule0P <* lookAhead "\n") $> 1) <|> (takeWhileP Nothing (/= '\n') $> 0)) `endBy` newline

main :: Text -> IO ()
main = withParser parser $ \input ->
  print @Integer . sum $ input
