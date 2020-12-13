{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Applicative ((<|>))
import           Data.List (sort, find)
import           Data.Maybe (catMaybes, isJust)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Input = Input { _inputEarliestDeparture :: Timestamp
                   , _inputBusIds :: [Maybe BusId]
                   } deriving (Show, Eq)

newtype BusId = BusId Int deriving (Show, Eq, Ord)

newtype Timestamp = Timestamp Int deriving (Show, Eq, Ord, Enum)

stopsAt :: BusId -> Timestamp -> Bool
stopsAt (BusId i) (Timestamp t) = t `mod` i == 0

main :: IO ()
main = do
  input <- TIO.getContents
  print $ parseInput input
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

calcSolution :: Timestamp -> (Timestamp, BusId) -> Int
calcSolution (Timestamp start) (Timestamp actual, BusId i) = (actual - start) * i

solvePart1 :: Input -> Maybe Int
solvePart1 (Input earliestDep ids) = do
  (t, maybeBusId) <- result
  busId <- maybeBusId
  pure $ calcSolution earliestDep (t, busId)
  where result = find (\case (_, maybeBusId) -> isJust maybeBusId)
               . map (\departure -> (departure, )
                                    . find (`stopsAt` departure)
                                    $ catMaybes ids)
               $ [earliestDep..]

solvePart2 :: Input -> _
solvePart2 _ = ()

parseInput :: Text -> Input
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec.Parsec Text () Input
parser = Input
     <$> depParser
     <*> idsParser
     <* Parsec.newline
     <* Parsec.eof
  where depParser = Timestamp <$> numberParser <* Parsec.newline
        idsParser = map (fmap BusId) . sort
                <$> Parsec.sepBy (Just <$> numberParser <|> Nothing <$ Parsec.char 'x')
                                 (Parsec.char ',')

numberParser :: Parsec Text () Int
numberParser = read @Int <$> Parsec.many1 Parsec.digit
