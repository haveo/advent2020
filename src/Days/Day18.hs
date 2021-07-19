module Days.Day18 where

import Common

exprP :: Num a => Parser a
exprP = do
  n <- operandP
  restP n
  where
    rest1P n = do
      op <- operatorP
      m <- operandP
      restP (n `op` m)
    restP n = rest1P n <|> pure n
    parenthesizedP = "(" *> exprP <* ")"
    operandP = parenthesizedP <|> decimal
    operatorP = " + " $> (+) <|> " * " $> (*)

exprP' :: Num a => Parser a
exprP' = do
  n <- operandP
  restP n
  where
    multP n = do
      _ <- " * "
      m <- exprP'
      pure $ n * m
    addP n = do
      _ <- " + "
      m <- operandP
      restP $ n + m
    restP n = multP n <|> addP n <|> pure n
    parenthesizedP = "(" *> exprP' <* ")"
    operandP = parenthesizedP <|> decimal

main :: Text -> IO ()
main = withParser (((,) <$> lookAhead exprP <*> exprP') `endBy` newline) $ \input -> do
  print @Integer . sum . fmap fst $ input
  print @Integer . sum . fmap snd $ input
