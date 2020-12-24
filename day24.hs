{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Data.Foldable (foldl')
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Semigroup (sconcat)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Coord = Coord !Int !Int !Int deriving (Show, Eq, Ord)

data Color = Black | White deriving (Show, Eq, Ord)

instance Semigroup Coord where
  Coord x1 y1 z1 <> Coord x2 y2 z2 = Coord (x1+x2) (y1+y2) (z1+z2)

data Dir = E
         | SE
         | SW
         | W
         | NW
         | NE
         deriving (Show, Eq, Enum)

dirToCoord :: Dir -> Coord
dirToCoord E = Coord    1   0 (-1)
dirToCoord W = Coord  (-1)  0   1
dirToCoord SE = Coord   0   1 (-1)
dirToCoord SW = Coord (-1)  1   0
dirToCoord NW = Coord   0 (-1)  1
dirToCoord NE = Coord   1 (-1)  0

intToColor :: Int -> Color
intToColor i = if even i then White else Black

main :: IO ()
main = do
  input <- TIO.getContents
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: NonEmpty (NonEmpty Dir) -> Int
solvePart1 = countBlackTiles . buildMap

countBlackTiles :: Map Coord Color -> Int
countBlackTiles = Map.foldl' f 0
  where f acc Black = acc + 1
        f acc White = acc

solvePart2 :: NonEmpty (NonEmpty Dir) -> Int
solvePart2 = countBlackTiles . (!!100) . iterate step . buildMap

buildMap :: NonEmpty (NonEmpty Dir) -> Map Coord Color
buildMap = fmap intToColor
         . Map.fromListWith (+)
         . NonEmpty.toList
         . fmap ((,1) . sconcat . fmap dirToCoord)

neighbors :: Coord -> [Coord]
neighbors baseCoord = map ((baseCoord <>) . dirToCoord) [E .. ]

lookupNeighbors :: Map Coord Color -> Coord -> [Color]
lookupNeighbors m = map (\neighbor -> Map.findWithDefault White neighbor m)
                  . neighbors

applyRule1 :: Color -> [Color] -> Color
applyRule1 Black nbs = if (||) <$> (==0) <*> (>2) $ length $ filter (==Black) nbs then White else Black
applyRule1 White nbs = if (==2) $ length $ filter (==Black) nbs then Black else White

step :: Map Coord Color -> Map Coord Color
step m = Map.foldlWithKey' f Map.empty . pad $ m
  where
    f :: Map Coord Color -> Coord -> Color -> Map Coord Color
    f acc k v = Map.insert k (applyRule1 v (lookupNeighbors m k)) acc

pad :: Map Coord Color -> Map Coord Color
pad m = foldl' (flip (Map.alter f)) m cellsToCheck
  where cellsToCheck = concatMap neighbors . Map.keys $ m
        f Nothing = Just White
        f x = x

parseInput :: Text -> NonEmpty (NonEmpty Dir)
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () (NonEmpty (NonEmpty Dir))
parser = NonEmpty.fromList <$> Parsec.many1 (lineParser <* Parsec.newline) <* Parsec.eof

lineParser :: Parsec Text () (NonEmpty Dir)
lineParser = NonEmpty.fromList <$> Parsec.many1 dirParser

dirParser :: Parsec Text () Dir
dirParser = Parsec.choice [ E <$ Parsec.string "e"
                          , SE <$ Parsec.try (Parsec.string "se")
                          , SW <$ Parsec.string "sw"
                          , W <$ Parsec.string "w"
                          , NW <$ Parsec.try (Parsec.string "nw")
                          , NE <$ Parsec.string "ne"
                          ]
