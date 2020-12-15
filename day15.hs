{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.List (unfoldr)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Value a = First a
             | Multiple a a
             deriving (Show, Eq)

newtype Turn = Turn { getTurn :: Integer} deriving (Show, Eq, Enum, Num)

data Memory = Memory Turn Integer (Map Integer (Value Turn))
  deriving (Show, Eq)

main :: IO ()
main = do
  input <- TIO.getContents
  print (parseInput input)
  print $ solvePart1 . parseInput $ input -- 1015
  print $ solvePart2 . parseInput $ input -- 201

initialMemory :: [Integer] -> Memory
initialMemory is =
  Memory (Turn $ fromIntegral (length is + 1))
         (last is)
         (Map.fromList (is `zip` map (First . Turn) [1..]))

solvePart1 :: [Integer] -> Integer
solvePart1 = last . take 2020 . play

solvePart2 :: [Integer] -> Integer
solvePart2 = last . take 30000000 . play

play :: [Integer] -> [Integer]
play is = is ++ unfoldr step (initialMemory is)

step :: Memory -> Maybe (Integer, Memory)
step mem@(Memory _ prev m) = case Map.lookup prev m of
  Just (First _) -> Just (0, updateMemory 0 mem)
  Just (Multiple t1 t2) -> Just (getTurn (t2-t1), updateMemory (getTurn $ t2-t1) mem)
  Nothing -> error "oops"

updateMemory :: Integer -> Memory -> Memory
updateMemory i (Memory turn _ m) = Memory (turn + 1) i (Map.alter alter i m)
  where
    alter Nothing = Just (First turn)
    alter (Just (First t)) = Just (Multiple t turn)
    alter (Just (Multiple _ t)) = Just (Multiple t turn)

parseInput :: Text -> [Integer]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Integer]
parser = number `Parsec.sepBy` Parsec.char ','
  <* Parsec.newline
  <* Parsec.eof
  where number = read @Integer <$> Parsec.many1 Parsec.digit
