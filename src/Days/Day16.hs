module Days.Day16 where

import Common
import Algebra.Lattice
import Algebra.Heyting
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List.NonEmpty (nonEmpty)
import Data.List (delete)

data Range a = Range a a
data Field a = Field Text (Range a) (Range a)

inRange :: Ord a => Range a -> a -> Bool
inRange (Range x y) z = z >= x && z <= y

validate :: Ord a => Field a -> a -> Set Text
validate (Field s r1 r2) = (\b -> if b then S.singleton s else S.empty) . (inRange r1 \/ inRange r2)

parser :: Num a => Parser ([Field a], [a], [[a]])
parser =
  (,,) <$> field `endBy` newline
  <* "\nyour ticket:\n" <*> ticket
  <* "\n\nnearby tickets:" <* newline <*> ticket `endBy` newline
  <* eof
 where
   field = Field <$> takeWhile1P Nothing ((/= ':') /\ (/= '\n'))  <* ": " <*> range <* " or " <*> range
   range = Range <$> decimal <* "-" <*> decimal
   ticket = decimal `sepBy` ","

simplify :: Map a (Set Text) -> Map a Text
simplify m =
  if length solved == M.size m then fmap (fromJust . fromSingleton) m
  else simplify $ fmap (\s -> case fromSingleton s of
                     Nothing -> S.difference s solved
                     Just _ -> s) m
  where
    solved = S.fromList $ mapMaybe fromSingleton (M.elems m)
    fromSingleton s = case S.toList s of
                        [x] -> Just x
                        _ -> Nothing

main :: Text -> IO ()
main = withParser parser $ \(fields, ticket, nearbyTickets) ->
  let validateAny = fmap (> bottom) . joins . fmap validate $ fields
      validateFields = joins . fmap validate $ fields
      validTickets = filter (meets  . fmap validateAny) nearbyTickets
      possibleFields :: Int -> Set Text
      possibleFields = meets1 . fromJust . nonEmpty $ ((validateFields .) . (!!) <$> validTickets)
      fieldNames = simplify . M.fromList . fmap (\i -> (i, possibleFields i)) $ [0..(length ticket - 1)]
  in do
    print @[Integer] $ backprop [S.fromList [1,2], S.fromList [2,3], S.fromList [2]]
    print @Integer . sum . filter (neg validateAny) . join $ nearbyTickets
    print . product . fmap (ticket !!) . M.keys . M.filter (isPrefixOf "departure") $ fieldNames

data Iter a where
  Now :: a -> Iter a
  Later :: Iter a -> Iter a

retract (Now a) = a
retract (Later w) = retract w

backprop :: forall a. Ord a => [Set a] -> [a]
backprop xs = retract <$> go where
  go = f (found go) <$> xs
  f _ (S.toList -> [x]) = Now x
  f ~(r:rs) s = Later (f rs (S.difference s r))
  step [] = (S.empty, [])
  step (x:xs) = case x of
    Now a -> (S.insert a s, vs)
    Later v -> (s, v:vs)
   where (s, vs) = step xs
  found xs = s : found vs
   where (s, vs) = step xs
