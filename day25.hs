{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.List (find)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

newtype PubKey = PubKey Integer
  deriving newtype (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  print  . parseInput $ input
  print . solvePart1 . parseInput $ input

solvePart1 :: (PubKey, PubKey) -> Integer
solvePart1 (PubKey card, PubKey door) = (!!loops) $ loop door
  where Just loops = fmap fst $ find (\case (_,num) -> num == card) . zip @Int [0..] $ loop 7

loop :: Integer -> [Integer]
loop subjectNumber = iterate (\x -> x * subjectNumber `rem` 20201227) 1

parseInput :: Text -> (PubKey, PubKey)
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () (PubKey, PubKey)
parser = (,) <$> pubKeyParser <* Parsec.newline
             <*> pubKeyParser <* Parsec.newline
             <* Parsec.eof

pubKeyParser :: Parsec Text () PubKey
pubKeyParser = PubKey . read @Integer <$> Parsec.many1 Parsec.digit
