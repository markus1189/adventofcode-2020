{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Monad.State as State
import           Data.Function (on)
import           Data.List (transpose, nubBy, find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

newtype TileId = TileId Int deriving (Show, Eq, Ord)

data Border = Border { borderTiles :: [Char]
                     , borderSide :: BorderSide
                     } deriving (Show, Eq, Ord)
data BorderSide = N | E | S | W deriving (Show, Eq, Ord)

data Op = FlipUpsideDown | FlipLeftRight | RotateLeft | Crop deriving (Show, Eq, Ord)

type TileGrid = [[Char]]

data Tile = Tile { tileId :: TileId
                 , tiles :: TileGrid
                 , tileOps :: [Op]
                 } deriving (Eq, Ord)

instance Show Tile where
  show t = showTileQuick t

showTileQuick :: Tile -> String
showTileQuick (Tile (TileId tid) _ os) = '{' : show tid <> "|" <> show os <> "}"

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

solvePart1 :: [Tile] -> Int
solvePart1 ts = Map.foldlWithKey (\acc (Tile (TileId k) _ _) _ -> acc * k) 1 $ Map.filter ((==2) . length) $ buildAdjMap ts

solvePart2 :: [Tile] -> Int
solvePart2 ts = countHash croppedFinalGrid - (numMonsters * countHash monsterPattern)
  where numMonsters = maximum $ map (countMonsters . tiles) $ allVersions $ Tile (TileId 42) croppedFinalGrid []
        croppedFinalGrid = assembleJigsawRows (map (map (tiles . cropTile)) (reverse arrangedTiles))
        arrangedTiles = flip State.evalState ts $ do
          nbs <- findMatchingTiles (tileId randomCorner)
          let Just horizontalDirection = snd <$> find (\case (_,dir) -> dir `elem` [W,E]) nbs
              Just verticalDirection = snd <$> find (\case (_,dir) -> dir `elem` [N,S]) nbs
          firstCol <- (randomCorner :) <$> unfoldTiles randomCorner verticalDirection
          traverse (\t -> (t:) <$> unfoldTiles t horizontalDirection) firstCol
        m = buildAdjMap ts
        corners = map fst $ filter ((==2).length.snd) $ Map.toList m
        Just randomCorner = listToMaybe corners

monsterPattern :: [String]
monsterPattern = [ "                  # "
                 , "#    ##    ##    ###"
                 , " #  #  #  #  #  #   "
                 ]

countHash :: [[Char]] -> Int
countHash = length . filter (=='#') . concat

monsterIndices :: [(Int,Int)]
monsterIndices = map fst $ filter (\case (_,c) -> c == '#') $ concat $ zipWith @Int (\row icols -> map (\case (col,c) -> ((row,col), c)) icols) [0..] $ map (zip @Int [0..]) monsterPattern

checkMonsterAt :: [String] -> (Int, Int) -> Bool
checkMonsterAt jigsaw (baseRow, baseCol) = all (\case (row,col) -> isFilledAt jigsaw (baseRow + row, baseCol + col)) monsterIndices

countMonsters :: [String] -> Int
countMonsters jigsaw = length $ filter (checkMonsterAt jigsaw) (indicesToCheck jigsaw)

indicesToCheck :: [String] -> [(Int, Int)]
indicesToCheck jigsaw = do
  row <- [0..rowsToCheck]
  col <- [0..colsToCheck]
  pure (row,col)
  where rowsJigsaw = length jigsaw
        colsJigsaw = length $ head jigsaw
        rowsMonster = length monsterPattern
        colsMonster = length $ head monsterPattern
        rowsToCheck = rowsJigsaw - rowsMonster `max` 0
        colsToCheck = colsJigsaw - colsMonster `max` 0

assembleJigsawRows :: [[[[a]]]] -> [[a]]
assembleJigsawRows = concatMap (map concat . transpose)

indexJigsaw :: [String] -> (Int, Int) -> Maybe Char
indexJigsaw jigsaw (row,col) = do
  r <- jigsaw !!? row
  r !!? col

isFilledAt :: [String] -> (Int, Int) -> Bool
isFilledAt jigsaw rc = Just True == ((=='#') <$> indexJigsaw jigsaw rc)

(!!?) :: [a] -> Int -> Maybe a
xs !!? i = if i < 0 || i >= length xs
                 then Nothing
                 else Just (xs !! i)

cropTile :: Tile -> Tile
cropTile (Tile tid cells os) = Tile tid (cropMatrix cells) (os ++ [Crop])

cropMatrix :: [[a]] -> [[a]]
cropMatrix = map (init . tail) . init . tail

unfoldTiles :: Tile -> BorderSide -> State [Tile] [Tile]
unfoldTiles corner dir = do
  nbs <- findMatchingTiles (tileId corner)
  let maybeNext = fst <$> find (\case (_,dir') -> dir == dir') nbs
  case maybeNext of
    Nothing -> pure []
    Just next -> (next:) <$> unfoldTiles next dir

buildAdjMap :: [Tile] -> Map Tile [(Tile, BorderSide)]
buildAdjMap ts = Map.fromListWith (++)
            . map (\case (t1,t2,b) -> (t1,[(t2,b)]))
            . catMaybes $ do
                t1 <- ts
                t2 <- ts
                guard $ t1 /= t2
                pure $ tryCombineTiles t1 t2

mirrored :: Tile -> [Tile]
mirrored t@(Tile tid ts os) = [ Tile tid (reverse ts) (tileOps t ++ [FlipUpsideDown])
                            , Tile tid (map reverse ts) (os ++ [FlipLeftRight])
                            , Tile tid (reverse (map reverse ts)) (os ++ [FlipUpsideDown, FlipLeftRight])
                            ]

allVersions :: Tile -> [Tile]
allVersions t = t : mirrored t ++ rotated t ++ concatMap mirrored (rotated t)

findMatchingTiles :: TileId -> State [Tile] [(Tile, BorderSide)]
findMatchingTiles tid = do
  availableTiles <- State.get
  let Just t = find ((== tid) . tileId) availableTiles
  let rotatedNbs = mapMaybe (tryCombineTiles' t) availableTiles
  State.modify $ nubBy ((==) `on` tileId) . (map fst rotatedNbs ++)
  pure rotatedNbs

tryCombineTiles' :: Tile -> Tile -> Maybe (Tile, BorderSide)
tryCombineTiles' t1 t2 = listToMaybe . catMaybes $ do
  t2' <- allVersions t2
  guard $ tileId t1 /= tileId t2'
  pure $ (t2',) . borderSide <$> checkBorders t1 t2'

tryCombineTiles :: Tile -> Tile -> Maybe (Tile, Tile, BorderSide)
tryCombineTiles t1 t2 = listToMaybe . catMaybes $ do
  t2' <- allVersions t2
  guard $ tileId t1 /= tileId t2'
  pure $ (t1,t2',) . (\case (Border _ side) -> side) <$> checkBorders t1 t2'

checkBorders :: Tile -> Tile -> Maybe Border
checkBorders t1 t2 = fst <$> find (\case (b1,b2) -> borderTiles b1 == borderTiles b2) commonBorders
  where bs1 = borders t1
        bs2 = borders t2
        commonBorders = [(head bs1, bs2 !! 2), (bs1 !! 1, bs2 !! 3), (bs1 !! 2, head bs2), (bs1 !! 3, bs2 !! 1)]

borders :: Tile -> [Border]
borders (Tile _ ts _) = map (uncurry Border) [ (head ts, N)
                                             , (last (transpose ts), E)
                                             , (last ts, S)
                                             , (head (transpose ts), W)
                                             ]

rotated :: Tile -> [Tile]
rotated = take 3 . drop 1 . iterate rotateLeft

rotateLeft :: Tile -> Tile
rotateLeft (Tile tid ts os) = Tile tid (reverse . transpose $ ts) (os ++ [RotateLeft])

parseInput :: Text -> [Tile]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Tile]
parser = Parsec.sepBy1 tileParser Parsec.newline

headerParser :: Parsec Text () TileId
headerParser = Parsec.string "Tile "
            *> (TileId . read @Int <$> Parsec.many1 Parsec.digit)
            <* Parsec.char ':'
            <* Parsec.newline

tileParser :: Parsec Text () Tile
tileParser = Tile
         <$> headerParser
         <*> Parsec.many1 (Parsec.many1 (Parsec.oneOf ".#") <* Parsec.newline)
         <*> pure []
