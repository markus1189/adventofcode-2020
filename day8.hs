{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Control.Monad.State (MonadState, gets, evalState, execState, runState)
import           Control.Monad (void)
import           Control.Lens.TH
import           Control.Lens.Operators
import           Control.Lens (view, use, _2)
import           Data.List (find)
import           Data.Functor (($>))
import           Control.Applicative ((<|>))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as Set

newtype Offset = Offset Int deriving (Show, Eq, Num)

newtype Address = Address Int deriving (Show, Eq, Ord, Num, Enum)

data Instr = Acc Int
           | Jmp Offset
           | Nop Offset
           deriving (Show, Eq)

newtype Program = Program [(Address, Instr)] deriving (Show, Eq)

data EvalState = EvalState { _evalStatePointer :: Address
                           , _evalStateAcc :: Int
                           , _evalStateSeen :: Set Address
                           } deriving (Show, Eq)
makeLenses ''EvalState

data Result = Loop | Terminate deriving (Show, Eq, Ord)

jump :: Offset -> Address -> Address
jump (Offset o) (Address a) = Address (a + o)

terminates :: Address -> Program -> Bool
terminates a (Program is) = fst (last is) + 1 == a

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right p -> do
      print $ solvePart1 p -- 1394
      print $ solvePart2 p -- 1626

initialState :: EvalState
initialState = EvalState 0 0 Set.empty

solvePart1 :: Program -> Int
solvePart1 p = view evalStateAcc $ execState (executeProgram p) initialState

solvePart2 :: Program -> Int
solvePart2 p@(Program is) =
  view (_2 . evalStateAcc) <$>
  find (\case (r,_) -> r == Terminate) (map (flip runState initialState . executeProgram) programs)
  where programs = map (\case (addr, _) -> modifyProgram addr p) is

modifyProgram :: Address -> Program -> Program
modifyProgram addr (Program is) = Program $ map go is
  where go (a,i) = if a == addr
                   then (a, flipInstruction i)
                   else (a, i)

flipInstruction :: Instr -> Instr
flipInstruction acc@(Acc _) = acc
flipInstruction (Jmp o) = Nop o
flipInstruction (Nop o) = Jmp o

executeProgram :: MonadState EvalState m => Program -> m Result
executeProgram p = do
  addr <- use evalStatePointer
  known <- gets atKnownAddress
  let done = terminates addr p
  case (known, done) of
    (True, _) -> pure Loop
    (False, True) -> pure Terminate
    _ -> do
      let instr = getInstruction p addr
      case instr of
        Nothing -> error $ "Invalid addr: " <> show addr
        Just i -> do
          evalInstr i
          void $ evalStateSeen %= Set.insert addr
          executeProgram p

atKnownAddress :: EvalState -> Bool
atKnownAddress = Set.member <$> view evalStatePointer <*> view evalStateSeen

getInstruction :: Program -> Address -> Maybe Instr
getInstruction (Program addrs) a = snd <$> find (\case (addr,_) -> addr == a) addrs

evalInstr :: MonadState EvalState m => Instr -> m ()
evalInstr (Acc i) = (evalStatePointer %= jump 1) *> (evalStateAcc += i)
evalInstr (Jmp o) = evalStatePointer %= jump o
evalInstr (Nop _) = evalStatePointer %= jump 1

parser :: Parsec Text () Program
parser = Program . zip [0..] <$> Parsec.many1 (instrParser <* Parsec.newline) <* Parsec.eof

signedNumParser :: Parsec Text () Int
signedNumParser = (*) <$> sign <*> num
  where sign = 1 <$ Parsec.char '+' <|> (-1) <$ Parsec.char '-'
        num = read @Int <$> Parsec.many1 Parsec.digit

instrParser :: Parsec Text () Instr
instrParser = Parsec.choice [ Parsec.string "acc" *> Parsec.space *> (Acc <$> signedNumParser)
                            , Parsec.string "jmp" *> Parsec.space *> (Jmp . Offset <$> signedNumParser)
                            , Parsec.string "nop" *> Parsec.space *> (Nop . Offset <$> signedNumParser)
                            ]
