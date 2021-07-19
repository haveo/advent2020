module Days.Day04 where

import Common
import Data.Map
import Data.Maybe

parseField :: Parser (Text, Bool)
parseField = do
  field <- pack <$> manyTill letterChar (char ':')
  value <- try (fromMaybe (fail "invalid field") (Data.Map.lookup field fieldsMap) *> lookAhead (char ' ' <|> char '\n') $> True)
    <|> (many (satisfy (\x -> x /= ' ' && x /= '\n')) $> False)
  _ <- char ' ' <|> char '\n'
  return (field, value)

parser :: Parser [Map Text Bool]
parser = (fromList <$> many parseField) `sepEndBy` newline

parseYear :: Int -> Int -> Parser ()
parseYear min max = do
  n <- decimal
  guard (n >= min)
  guard (n <= max)
  return ()

parseHeight :: Parser ()
parseHeight = do
  n :: Integer <- decimal
  unit <- chunk "cm" <|> chunk "in"
  case unit of
    "cm" -> guard (n >= 150) >> guard (n <= 193) >> return ()
    "in" -> guard (n >= 59) >> guard (n <= 76) >> return ()

parseHair :: Parser ()
parseHair = do
  _ <- char '#'
  _ <- count 6 (satisfy (`elem` (['0'..'9'] ++ ['a'..'f'])))
  return ()

colors :: [Text]
colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

parseEye :: Parser ()
parseEye = do
  color <- pack <$> many letterChar
  guard (color `elem` colors)
  return ()

parseId :: Parser ()
parseId = do
  _ <- count 9 digitChar
  return ()

fieldsMap :: Map Text (Parser ())
fieldsMap = fromList
  [ ("byr", parseYear 1920 2020)
  , ("iyr", parseYear 2010 2020)
  , ("eyr", parseYear 2020 2030)
  , ("hgt", parseHeight)
  , ("hcl", parseHair)
  , ("ecl", parseEye)
  , ("pid", parseId)
  ]

main :: Text -> IO ()
main = withParser parser $ \input -> do
  print . howMany (Data.Map.null . difference fieldsMap) $ input
  print . howMany (Data.Map.null . difference fieldsMap) $ fmap (Data.Map.filter id) input
