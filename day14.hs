{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.List (foldl')
import           Control.Applicative ((<|>))
import           Data.Bits (testBit)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data MaskBit = One | Zero | Floating deriving (Show, Eq, Ord)

newtype Mask = Mask [MaskBit] deriving (Show, Eq, Ord)

newtype Address = Address { getAddress :: Integer } deriving (Show, Eq, Ord)

newtype Value = Value { getValue :: Integer} deriving (Show, Eq, Ord)

data Instruction = SetMask Mask
                 | SetMem Address Value
                 deriving (Show, Eq, Ord)

data MemoryState = MemoryState { memoryAddressSpace :: Map Address Value
                               , memoryMask :: Maybe Mask
                               } deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input -- 14839536808842
  print $ solvePart2 . parseInput $ input -- 4215284199669

solvePart1 :: [Instruction] -> Integer
solvePart1 = foldl' (+) 0
           . map getValue
           . Map.elems
           . memoryAddressSpace
           . foldl' applyInstructionPart1 initialMemory

solvePart2 :: [Instruction] -> Integer
solvePart2 = foldl' (+) 0
           . map getValue
           . Map.elems
           . memoryAddressSpace
           . foldl' applyInstructionPart2 initialMemory

initialMemory :: MemoryState
initialMemory = MemoryState Map.empty Nothing

applyMaskToValue :: Mask -> Value -> Value
applyMaskToValue (Mask bits) v =
  Value $ bitsToInteger $ zipWith zipBit bits (integerToBits (getValue v))
  where
    zipBit One _ = True
    zipBit Zero _ = False
    zipBit Floating b = b

applyMaskToAddress :: Mask -> Address -> [Address]
applyMaskToAddress (Mask bits) (Address addr) =
  map (Address . bitsToInteger)
  . sequence
  . map mapBit
  . zipWith zipBit bits
  . map (\b -> if b then One else Zero)
  $ integerToBits addr
  where
    zipBit Zero b = b
    zipBit One _ = One
    zipBit Floating _ = Floating

    mapBit Zero = [False]
    mapBit One = [True]
    mapBit Floating = [False, True]

applyInstructionPart1 :: MemoryState -> Instruction -> MemoryState
applyInstructionPart1 (MemoryState space _) (SetMask m) = MemoryState space (Just m)
applyInstructionPart1 (MemoryState space Nothing) (SetMem addr v) = MemoryState (Map.insert addr v space) Nothing
applyInstructionPart1 (MemoryState space (Just m)) (SetMem addr v) = MemoryState space' (Just m)
  where space' = Map.insert addr v' space
        v' = applyMaskToValue m v

applyInstructionPart2 :: MemoryState -> Instruction -> MemoryState
applyInstructionPart2 (MemoryState space _) (SetMask m) = MemoryState space (Just m)
applyInstructionPart2 (MemoryState space Nothing) (SetMem addr v) = MemoryState (Map.insert addr v space) Nothing
applyInstructionPart2 (MemoryState space (Just m)) (SetMem addr v) = MemoryState space' (Just m)
  where space' = foldl' (\acc addr' -> Map.insert addr' v acc) space addresses
        addresses = applyMaskToAddress m addr

integerToBits :: Integer -> [Bool]
integerToBits i = map (testBit i) [35,34..0]

bitsToInteger :: [Bool] -> Integer
bitsToInteger bs =
  foldr (\indexedBit acc ->
            acc + if snd indexedBit
                  then 2 ^ fst indexedBit
                  else 0)
        0
        (zip @Int [0..] (reverse bs))

parseInput :: Text -> [Instruction]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec.Parsec Text () [Instruction]
parser = Parsec.many1 (instructionParser <* Parsec.newline) <* Parsec.eof

instructionParser :: Parsec Text () Instruction
instructionParser = Parsec.try setMaskParser <|> assignMemParser

setMaskParser :: Parsec Text () Instruction
setMaskParser = SetMask . Mask <$> (Parsec.string "mask = " *> Parsec.many1 maskBitParser)

maskBitParser :: Parsec Text () MaskBit
maskBitParser = Parsec.choice [ Zero <$ Parsec.char '0'
                              , One <$ Parsec.char '1'
                              , Floating <$ Parsec.char 'X'
                              ]

assignMemParser :: Parsec Text () Instruction
assignMemParser = SetMem <$> (Address <$> addr) <*> (Value <$> value)
  where
    addr = Parsec.string "mem" *> Parsec.char '[' *> numberParser <* Parsec.char ']'
    value = Parsec.string " = " *> numberParser

numberParser :: Parsec Text () Integer
numberParser = read @Integer <$> Parsec.many1 Parsec.digit
