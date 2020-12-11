{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
import           Debug.Trace (traceShowM, traceShowId, traceShow)
import           Data.List.Split (chunksOf)
import           Control.Monad (guard)
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Data.Proxy
import           Text.Parsec (Parsec)
import           Data.Text (Text)
import qualified Math.Geometry.Grid as Grid
import           Data.List (intercalate)
import qualified Math.Geometry.GridMap as GridMap
import           Math.Geometry.GridMap.Lazy (lazyGridMap, LGridMap)
import           Math.Geometry.Grid.Square (rectSquareGrid, RectSquareGrid)
import           Data.Functor.Rep (Representable(..), distributeRep)
import           Data.Distributive (Distributive(..))
import           GHC.TypeLits
import           Control.Comonad.Representable.Store (StoreT(..), Store, store, experiment)
import           Control.Comonad (extract, extend)
import           Data.Functor.Identity (Identity(..))
import           Data.Map (Map)
import qualified Data.Map as Map

data Tile = Floor | EmptySeat | Occupied deriving (Show, Eq)

newtype FixedGridMap (r :: Nat) (c :: Nat) a = FixedGridMap (LGridMap RectSquareGrid a)
  deriving (Show, Eq)
  deriving Functor via (LGridMap RectSquareGrid)

mkFixedGridMap :: Proxy (r :: Nat) -> Proxy (c :: Nat) -> LGridMap RectSquareGrid a -> FixedGridMap r c a
mkFixedGridMap _ _ = FixedGridMap

instance (KnownNat r, KnownNat c) => Distributive (FixedGridMap r c) where
  distribute = distributeRep

instance forall r c. (KnownNat r, KnownNat c) => Representable (FixedGridMap r c) where
  type Rep (FixedGridMap r c) = (Int, Int)
  tabulate f = let rows = fromIntegral $ natVal (Proxy @r)
                   cols = fromIntegral $ natVal (Proxy @c)
                   grid = rectSquareGrid rows cols
                   gridMap = lazyGridMap grid . map f $ [ (row, col) | row <- [0..(rows-1)], col <- [0..(cols-1)]]
               in FixedGridMap gridMap

  index (FixedGridMap g) (row,col) =
    let rows = fromIntegral $ natVal (Proxy @r)
        cols = fromIntegral $ natVal (Proxy @c)
    in g GridMap.! (row `mod` rows, col `mod` cols)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

showFixedGridMap (FixedGridMap gm) = intercalate "\n" $ chunksOf cols $ map (showTile . (gm GridMap.!)) [(row, col) | row <- [0..(rows-1)], col <- [0..(cols-1)]]
  where (rows, cols) = (\case (x,y) -> (fromIntegral x, fromIntegral y)) $ Grid.size gm

showTile Floor = '.'
showTile EmptySeat = 'L'
showTile Occupied = '#'

solvePart1 :: LGridMap RectSquareGrid Tile -> Maybe _
solvePart1 gm = do
  SomeNat r <- someNatVal rows
  SomeNat c <- someNatVal cols
  let gridStore = mkStore $ mkFixedGridMap r c gm
      (FixedGridMap gm', n) = fixpoint 0 gridStore
  pure $ (n, Map.foldl' (\acc x -> if x == Occupied then acc + 1 else acc) 0 $ GridMap.toMap gm')
  where (rows, cols) = (\case (x,y) -> (fromIntegral x, fromIntegral y)) $ Grid.size gm

fixpoint :: forall r c. (KnownNat r, KnownNat c) => Int -> Store (FixedGridMap r c) Tile -> (FixedGridMap r c Tile,  Int)
fixpoint x s@(StoreT (Identity fgm) _) = if fgm == fgm' then (fgm, x) else fixpoint (x+1) s'
  where s'@(StoreT (Identity fgm') _) = extend applyRule s

solvePart2 :: LGridMap RectSquareGrid Tile -> _
solvePart2 _ = ()

applyRule :: forall r c. (KnownNat r, KnownNat c) => Store (FixedGridMap r c) Tile -> Tile
applyRule fgm = case focus of
  EmptySeat -> if Occupied `notElem` nbs then Occupied else focus
  Occupied -> if (>3) . length . filter (== Occupied) $ nbs then EmptySeat else focus
  _ -> focus
  where nbs = experiment (\input -> mkNeighbors (fromIntegral $ natVal (Proxy @r), fromIntegral $ natVal (Proxy @r)) $ input) fgm
        focus = extract fgm

mkStore :: (KnownNat r, KnownNat c) => FixedGridMap r c Tile -> Store (FixedGridMap r c) Tile
mkStore (FixedGridMap gm) = store (\case (r,c) -> gm GridMap.! (r,c)) (0,0)

mkNeighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
mkNeighbors (rows, cols) (r, c) = do
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ (dr,dc) /= (0,0)
  let x@(r', c') = (r + dr, c + dc)
  guard $ r' >= 0 && r' < rows && c' < cols && c' >= 0
  pure x

parseInput :: Text -> LGridMap RectSquareGrid Tile
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right tiles ->
      let cols = length . head $ tiles
          rows = length tiles
          grid = rectSquareGrid rows cols
          gridMap = lazyGridMap grid (concat tiles)
      in gridMap

parser :: Parsec.Parsec Text () [[Tile]]
parser = Parsec.many1 rowParser <* Parsec.eof

rowParser :: Parsec Text () [Tile]
rowParser = Parsec.many1 tileParser <* Parsec.newline

tileParser :: Parsec Text () Tile
tileParser = Parsec.choice [ Floor <$ Parsec.char '.'
                           , EmptySeat <$ Parsec.char 'L'
                           , Occupied <$ Parsec.char '#'
                           ]
