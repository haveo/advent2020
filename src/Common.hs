module Common (assert, listToMaybe, bool, isPrefixOf, forM_, Data.Maybe.mapMaybe, complement, (.|.), (.&.), eitherP, oneOf, catMaybes, ($>),(<$), Control.Monad.State.State, bimap, Data.Foldable.find, isRight, evalState, get, coerce, MonadState, modify, signed, choice, ala', optional, second, fix, (!), Prelude.any, eof, HasCallStack, fromJust, Map, Set, join, sepBy, some, ord, howMany, count, try, digitChar, lookAhead, satisfy, sepEndBy, pack, takeWhile1P, space, printChar, letterChar, manyTill, countTrue, (*>), (<|>), char, many, both, guard, withParser, Parser, Stream, Text, decimal, endBy, newline, lowerChar, chunk, takeWhileP, unpack) where

import Control.Monad
import Control.Exception hiding (try)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Function
import Data.Bits
import Data.Void
import Data.Text hiding (count)
import Data.Bifunctor
import Data.Char
import Data.Set
import Data.Functor
import Data.Map
import Data.Maybe
import GHC.Stack
import Data.Proxy
import Data.Coerce
import Control.Monad.State
import Data.Either
import Data.Foldable
import Data.Bool
import Data.Maybe

type Parser = Parsec Void Text

infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
{-# INLINE (#.) #-}

ala' :: (Coercible a b, Coercible a' b')
     => (a -> b)
     -> ((d -> b) -> c -> b')
     -> (d -> a)
     -> c
     -> a'
ala' _ hof f = coerce #. hof (coerce f)
{-# INLINE ala' #-}

howMany :: (a -> Bool) -> [a] -> Integer
howMany f = toInteger . Prelude.length . Prelude.filter f

countTrue False = 0
countTrue True = 1

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

withParser :: Parser a -> (a -> IO ()) -> Text -> IO ()
withParser p main s =
  case parse p "input" s of
    Left err -> print err
    Right v -> main v


