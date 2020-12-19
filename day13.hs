{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Applicative ((<|>))
import           Data.List (find, foldl')
import           Data.Maybe (catMaybes, isJust, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Input = Input { _inputEarliestDeparture :: Timestamp
                   , _inputBusIds :: [Maybe BusId]
                   } deriving (Show, Eq)

newtype BusId = BusId { getBusId :: Integer} deriving (Show, Eq, Ord)

newtype Timestamp = Timestamp Integer deriving (Show, Eq, Ord, Enum)

stopsAt :: BusId -> Timestamp -> Bool
stopsAt (BusId i) (Timestamp t) = t `mod` i == 0

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input -- 161
  print $ solvePart2 . parseInput $ input -- 213890632230818

calcSolution :: Timestamp -> (Timestamp, BusId) -> Integer
calcSolution (Timestamp start) (Timestamp actual, BusId i) = (actual - start) * i

solvePart1 :: Input -> Maybe Integer
solvePart1 (Input earliestDep ids) = do
  (t, maybeBusId) <- result
  busId <- maybeBusId
  pure $ calcSolution earliestDep (t, busId)
  where result = find (\case (_, maybeBusId) -> isJust maybeBusId)
               . map (\departure -> (departure, )
                                    . find (`stopsAt` departure)
                                    $ catMaybes ids)
               $ [earliestDep..]

solvePart2 :: Input -> Integer
solvePart2 (Input _ bs) = searchSolution lbids (getBusId . snd . head $ lbids) 1
  where lbids = labelledBusIds bs

search :: Integer -> Integer -> [(Integer, BusId)] -> Maybe Integer
search startValue multipleOf constraints = find (isSolution constraints) $ iterate (+multipleOf) startValue

searchSolution :: [(Integer, BusId)] -> Integer -> Int -> Integer
searchSolution lbids x n = if n == length lbids - 1
                           then intermediateSolution
                           else searchSolution lbids intermediateSolution (n+1)
  where is = map (getBusId . snd) lbids
        Just intermediateSolution = search x (lcms (take n is)) (take (n+1) lbids)

labelledBusIds :: [Maybe BusId] -> [(Integer, BusId)]
labelledBusIds = mapMaybe (\case (i, mb) -> (i,) <$> mb) . zip [0..]

lcms :: Foldable f => f Integer -> Integer
lcms = foldl' lcm 1

isSolution :: [(Integer, BusId)] -> Integer -> Bool
isSolution busIdsWithIndex t =
  all (\case (i, BusId b) -> ((t+i) `mod` b == 0)) busIdsWithIndex

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
        idsParser = map (fmap BusId)
                <$> Parsec.sepBy (Just <$> numberParser <|> Nothing <$ Parsec.char 'x')
                                 (Parsec.char ',')

numberParser :: Parsec Text () Integer
numberParser = read @Integer <$> Parsec.many1 Parsec.digit
