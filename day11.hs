{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
import           Control.Monad (guard)
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Data.Proxy
import           Text.Parsec (Parsec)
import           Data.Text (Text)
import qualified Math.Geometry.Grid as Grid
import           Data.List (unfoldr)
import qualified Math.Geometry.GridMap as GridMap
import           Math.Geometry.GridMap.Lazy (lazyGridMap, LGridMap)
import           Math.Geometry.Grid.Square (rectSquareGrid, RectSquareGrid)
import           Data.Functor.Rep (Representable(..), distributeRep)
import           Data.Distributive (Distributive(..))
import           GHC.TypeLits
import           Control.Comonad.Representable.Store (StoreT(..), Store, store, experiment)
import           Control.Comonad (extract, extend)
import           Data.Functor.Identity (Identity(..))
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
                   gridMap = lazyGridMap grid . map f $ offsets rows cols
               in FixedGridMap gridMap

  index (FixedGridMap g) (row,col) =
    let rows = fromIntegral $ natVal (Proxy @r)
        cols = fromIntegral $ natVal (Proxy @c)
    in g GridMap.! (row `mod` rows, col `mod` cols)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input -- 2427
  print $ solvePart2 . parseInput $ input -- 2199

solvePart1 :: LGridMap RectSquareGrid Tile -> Maybe Int
solvePart1 = genericSolve 4 mkNeighbors

solvePart2 :: LGridMap RectSquareGrid Tile -> Maybe Int
solvePart2 = genericSolve 5 visibleNeighbors

genericSolve :: Int
             -> (LGridMap RectSquareGrid Tile -> (Int, Int) -> [(Int, Int)])
             -> LGridMap RectSquareGrid Tile
             -> Maybe Int
genericSolve n findNbs gm = do
  SomeNat r <- someNatVal rows
  SomeNat c <- someNatVal cols
  let gridStore = mkStore $ mkFixedGridMap r c gm
      FixedGridMap gm' = fixpoint n findNbs gridStore
  pure $ Map.foldl' (\acc x -> if x == Occupied then acc + 1 else acc) 0 $ GridMap.toMap gm'
  where (rows, cols) = (\case (x,y) -> (fromIntegral x, fromIntegral y)) $ Grid.size gm

fixpoint :: forall r c. (KnownNat r, KnownNat c)
         => Int
         -> (LGridMap RectSquareGrid Tile -> (Int, Int) -> [(Int, Int)])
         -> Store (FixedGridMap r c) Tile
         -> FixedGridMap r c Tile
fixpoint n findNbs s@(StoreT (Identity fgm) _) =
  if fgm == fgm' then fgm else fixpoint n findNbs s'
  where s'@(StoreT (Identity fgm') _) = extend (applyRule n) s

offsets :: Int -> Int -> [(Int, Int)]
offsets rows cols = [ (row, col) | row <- [0..(rows-1)], col <- [0..(cols-1)]]

applyRule :: forall r c. (KnownNat r, KnownNat c) => Int -> Store (FixedGridMap r c) Tile -> Tile
applyRule n fgm@(StoreT (Identity (FixedGridMap gm)) _) = case focus of
  EmptySeat -> if Occupied `notElem` nbs then Occupied else focus
  Occupied -> if (>=n) . length . filter (== Occupied) $ nbs then EmptySeat else focus
  _ -> focus
  where nbs = experiment (mkNeighbors gm) fgm
        focus = extract fgm

mkStore :: (KnownNat r, KnownNat c) => FixedGridMap r c Tile -> Store (FixedGridMap r c) Tile
mkStore (FixedGridMap gm) = store (\case (r,c) -> gm GridMap.! (r,c)) (0,0)

mkNeighbors :: LGridMap RectSquareGrid Tile -> (Int, Int) -> [(Int, Int)]
mkNeighbors gm (r, c) = do
  let (rows, cols) = Grid.size gm
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  guard $ (dr,dc) /= (0,0)
  let x@(r', c') = (r + dr, c + dc)
  guard $ r' >= 0 && r' < rows && c' < cols && c' >= 0
  pure x

visibleNeighbors :: LGridMap RectSquareGrid Tile -> (Int, Int) -> [(Int, Int)]
visibleNeighbors gm (r, c) = concatMap (pick . peek) [(x,y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x,y) /= (0,0)]
  where peek (dx, dy) = unfoldr (\case (r', c') -> if gm `Grid.contains` (r', c')
                                                   then Just (((r', c'), gm GridMap.! (r', c')), (r' + dx, c' + dy))
                                                   else Nothing)
                                (r + dx, c + dy)
        pick [] = []
        pick [(crd,_)] = [crd]
        pick ((_,Floor):cs) = pick cs
        pick ((crd, EmptySeat):_) = [crd]
        pick ((crd, Occupied):_) = [crd]

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
