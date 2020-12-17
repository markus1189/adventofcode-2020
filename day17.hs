{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- EXPLORE: make more performant

import           Control.Applicative ((<|>))
import           Control.Monad (replicateM)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Coord = Coord { _cX :: Int, _cY :: Int, _cZ :: Int, _cW :: Int } deriving (Eq, Ord)

instance Show Coord where
  show (Coord x y z w) = show (x,y,z,w)

data Tile = Active | Inactive deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input -- 368
  print $ solvePart2 . parseInput $ input -- 2696

solvePart1 :: [[Tile]] -> Int
solvePart1 = countActive . genericSolve 6 neighbors3

solvePart2 :: [[Tile]] -> Int
solvePart2 = countActive . genericSolve 6 neighbors4

genericSolve :: Int -> (Coord -> [Coord]) -> [[Tile]] -> Map Coord Tile
genericSolve n genNbs = (!!n) . iterate (applyRuleEverywhere genNbs) . initialMap

initialMap :: [[Tile]] -> Map Coord Tile
initialMap = Map.fromList
           . concatMap (\case (y, row) -> map (\case (x, tile) -> (Coord x y 0 0, tile)) row)
           . zip @Int [0..]
           . map (zip @Int [0..])

countActive :: Map Coord Tile -> Int
countActive = Map.size . Map.filter (==Active)

applyRuleEverywhere :: (Coord -> [Coord]) -> Map Coord Tile -> Map Coord Tile
applyRuleEverywhere genNbs m = foldl' (applyRuleAtCoord genNbs m) m
                             . nubOrd
                             . concatMap genNbs
                             $ Map.keys m

applyRuleAtCoord :: (Coord -> [Coord]) -> Map Coord Tile -> Map Coord Tile -> Coord -> Map Coord Tile
applyRuleAtCoord genNbs mRead mWrite c = Map.alter f c mWrite
  where nbs = genNbs c
        f :: Maybe Tile -> Maybe Tile
        f (Just t) = Just $ applyRule t (lookupAll mRead nbs)
        f Nothing = Just $ applyRule Inactive (lookupAll mRead nbs)

applyRule :: Tile -> [Tile] -> Tile
applyRule Active nbs = if (`elem` [2,3]). length . filter (== Active) $ nbs
                       then Active
                       else Inactive
applyRule Inactive nbs = if (==3). length . filter (== Active) $ nbs
                         then Active
                         else Inactive

lookupAll :: Map Coord Tile -> [Coord] -> [Tile]
lookupAll m = map (\c -> Map.findWithDefault Inactive c m)

deltas :: Int -> [[Int]]
deltas n = filter (any (/=0)) $ replicateM n [-1,0,1]

neighbors3 :: Coord -> [Coord]
neighbors3 (Coord x y z w)= do
  [dx,dy,dz] <- deltas 3
  pure $ Coord (x+dx) (y+dy) (z+dz) w

neighbors4 :: Coord -> [Coord]
neighbors4 (Coord x y z w)= do
  [dx,dy,dz,dw] <- deltas 4
  pure $ Coord (x+dx) (y+dy) (z+dz) (w+dw)

parseInput :: Text -> [[Tile]]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [[Tile]]
parser = Parsec.many1 lineParser <* Parsec.eof
  where lineParser = Parsec.many1 tileParser <* Parsec.newline

tileParser :: Parsec Text () Tile
tileParser = Inactive <$ Parsec.char '.' <|> Active <$ Parsec.char '#'
