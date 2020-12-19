{-# LANGUAGE TypeApplications #-}
import           Data.List (foldl', sort, find)
import           Control.Monad (replicateM, unless)
import           Control.Applicative ((<|>))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)

data Row = Front | Back deriving (Show, Eq, Ord)
data Column = ColLeft | ColRight deriving (Show, Eq, Ord)

data Seat = Seat [Row] [Column] deriving (Show, Eq, Ord)

rowToInt :: Row -> Int
rowToInt Front = 0
rowToInt Back = 1

colToInt :: Column -> Int
colToInt ColLeft = 0
colToInt ColRight = 1

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser parser () "stdin" input of
    Left  e      -> error (show e)
    Right seats -> do
      unless (null seats) $ do
        print $ solvePart1 seats -- 951
        print $ solvePart2 seats -- 653

solvePart1 :: [Seat] -> Int
solvePart1 = foldl' (\acc x -> acc `max` calcSeat x) 0

solvePart2 :: [Seat] -> Maybe Int
solvePart2 seats = (+1) . fst <$> find correctGap pairs
  where ids = map calcSeat seats
        sortedIds = sort ids
        pairs = sortedIds `zip` tail sortedIds
        correctGap (x1,x2) = x2 - x1 == 2

calcSeat :: Seat -> Int
calcSeat (Seat rs cs) = foldr (\ib acc -> (snd ib * (2 ^ fst ib)) + acc) 0 (zip @Int [n,n-1..0] is)
  where is = map rowToInt rs ++ map colToInt cs
        n = length is - 1

parser :: Parsec Text () [Seat]
parser = Parsec.many1 lineParser <* Parsec.eof

lineParser :: Parsec Text () Seat
lineParser = Seat
         <$> rowInstructionsParser
         <*> columnInstructionsParser <* Parsec.newline

rowInstructionsParser :: Parsec Text () [Row]
rowInstructionsParser =
  replicateM 7 (Front <$ Parsec.char 'F' <|> Back <$ Parsec.char 'B')

columnInstructionsParser :: Parsec Text () [Column]
columnInstructionsParser =
  replicateM 3 (ColLeft <$ Parsec.char 'L' <|> ColRight <$ Parsec.char 'R')
