{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Control.Applicative ((<|>))
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Math.Geometry.GridMap.Lazy as GM
import           Math.Geometry.GridMap ((!), GridMap, BaseGrid)
import           Math.Geometry.Grid as Grid
import           Math.Geometry.Grid.Square (torSquareGrid)
import qualified Text.Parsec as Parsec

data Tile = Empty | Tree deriving (Show, Eq)

isTree :: Tile -> Bool
isTree Empty = False
isTree Tree = True

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser (parser <* Parsec.eof) () "stdin" input of
    Left  e      -> error (show e)
    Right r -> do
      let rows = length r
          cols = length . head $ r
          grid = torSquareGrid rows cols
          gridMap = GM.lazyGridMapIndexed grid (concat r)
      print . walk $ gridMap

walk :: (FiniteGrid (gm Tile),
         GridMap gm Tile,
         Index (BaseGrid gm Tile) ~ (Int, Int), Size (gm Tile) ~ (Int, Int))
     => gm Tile
     -> Int
walk gm = go (0, 0) 0
  where (maxRow, _) = Grid.size gm
        go (row, col) trees =
          if row == maxRow
          then trees
          else let newPos = (row + 1, col + 3)
                   trees' = if isTree (gm `wrappingLookup` (row, col))
                            then trees + 1
                            else trees
               in go newPos trees'

parser :: Parsec.ParsecT T.Text u Identity [[((Int, Int), Tile)]]
parser =
  zipWith (\row xs -> map (\case (col, t) -> ((row,col), t)) xs) [(0::Int)..] <$>
    Parsec.many1 (zip [(0::Int)..] <$>
                  Parsec.many1 parseTile <* Parsec.newline)

parseTile :: Parsec.ParsecT T.Text u Identity Tile
parseTile =
  (Empty <$ Parsec.char '.') <|> (Tree <$ Parsec.char '#')


wrappingLookup :: (FiniteGrid (gm v),
                   Integral a,
                   Integral b,
                   GridMap gm v,
                   Show a,
                   Show b,
                   Index (BaseGrid gm v) ~ (a, b),
                   Size (gm v) ~ (a, b))
               => gm v
               -> (a, b)
               -> v
wrappingLookup g (r,c) = g ! (r',c')
  where (rows,cols) = Grid.size g
        (r',c') = (r `mod` rows, c `mod` cols)
