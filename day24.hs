{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Data.Foldable (foldl')
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup (sconcat)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           GHC.Generics (Generic)
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Coord = Coord !Int !Int !Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass Hashable


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

countBlackTiles :: HashMap Coord Color -> Int
countBlackTiles = HashMap.foldl' f 0
  where f acc Black = acc + 1
        f acc White = acc

solvePart2 :: NonEmpty (NonEmpty Dir) -> Int
solvePart2 = countBlackTiles . (!!100) . iterate step . buildMap

buildMap :: NonEmpty (NonEmpty Dir) -> HashMap Coord Color
buildMap = fmap intToColor
         . HashMap.fromListWith (+)
         . NonEmpty.toList
         . fmap ((,1) . sconcat . fmap dirToCoord)

neighbors :: Coord -> [Coord]
neighbors baseCoord = map ((baseCoord <>) . dirToCoord) [E .. ]

lookupNeighbors :: HashMap Coord Color -> Coord -> [Color]
lookupNeighbors m = map (\neighbor -> HashMap.findWithDefault White neighbor m) . neighbors

applyRule1 :: Color -> [Color] -> Color
applyRule1 Black nbs = if (||) <$> (==0) <*> (>2) $ length $ filter (==Black) nbs then White else Black
applyRule1 White nbs = if (==2) $ length $ filter (==Black) nbs then Black else White

step :: HashMap Coord Color -> HashMap Coord Color
step m = HashMap.foldlWithKey' f HashMap.empty . pad $ m
  where
    f :: HashMap Coord Color -> Coord -> Color -> HashMap Coord Color
    f acc k v = HashMap.insert k (applyRule1 v (lookupNeighbors m k)) acc

pad :: HashMap Coord Color -> HashMap Coord Color
pad m = foldl' (flip (HashMap.alter f)) m cellsToCheck
  where cellsToCheck = concatMap neighbors . HashMap.keys . HashMap.filter (== Black) $ m
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
