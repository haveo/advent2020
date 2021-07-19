module Days.Day08 where

import Common
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Applicative
import Control.Monad

data Opcode = Nop | Acc | Jmp
  deriving Show

data Instruction = Instruction
  { _opcode :: Opcode
  , _arg :: Integer
  }
  deriving Show

newtype PC = PC Integer
  deriving (Show, Num, Eq, Ord, Enum)

newtype R = R Integer
  deriving (Show, Num)

data Mem = Mem
  { _pc :: PC
  , _r :: R
  } deriving (Show)

makeLenses ''Mem
makeLenses ''Instruction

runInstruction :: MonadState Mem m => Instruction -> m ()
runInstruction (Instruction Nop _) = pc += 1
runInstruction (Instruction Acc n) = pc += 1 >> r += R n
runInstruction (Instruction Jmp n) = pc += PC n

parserOpcode :: Parser Opcode
parserOpcode = choice [Nop <$ "nop", Acc <$ "acc", Jmp <$ "jmp"]

parser :: Parser (Map PC Instruction)
parser = M.fromAscList . zip [0..] <$>
  (Instruction <$> parserOpcode <* " " <*> signed (return ()) decimal) `endBy` newline

data M f g a = M
  { no :: f a
  , yes :: g (f a)
  } deriving Functor

instance (Monad f, Traversable f, Alternative g) => Monad (M f g) where
  (M xs ys) >>= f = M
    (xs >>= no . f)
    ((join <$> traverse (yes . f) xs) <|> ((>>= no . f) <$> ys))

instance (Monad f, Traversable f, Alternative g) => Applicative (M f g) where
  pure x = M (pure x) empty
  liftA2 = liftM2

changeOne :: Traversable t => (a -> [a]) -> t a -> [t a]
changeOne f = fmap runIdentity . yes . traverse (\x -> M (Identity x) (return <$> f x))

changeOpcode :: Opcode -> [Opcode]
changeOpcode Nop = [Jmp]
changeOpcode Acc = []
changeOpcode Jmp = [Nop]

main :: Text -> IO ()
main = withParser parser $ \input -> do
  let go :: MonadState Mem m => Set PC -> Map PC Instruction -> m (Either R R)
      go visited program = do
        x <- use pc
        if x `S.member` visited then Left <$> use r
        else maybe
          (Right <$> use r)
          (\ins -> runInstruction ins >> go (S.insert x visited) program)
          (M.lookup x program :: Maybe Instruction)
      programs :: [Map PC Instruction]
      programs = changeOne
                   (\(Instruction op n) -> (`Instruction` n) <$> changeOpcode op)
                   input
      results = flip evalState (Mem 0 0) . go S.empty <$> programs
  print . filter isRight $ results
  return ()
