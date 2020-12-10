{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
import           Data.Function ( on )
import           Data.List ( sort, tails, foldl')
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec ( Parsec )
import           Data.Text ( Text )
import           Test.Tasty
import           Test.Tasty.Hspec
import qualified Data.Map as Map

newtype Adapter = Adapter { adapterJolt :: Int
                          } deriving (Show, Eq, Ord)

data Count = Count { _countThrees :: Int
                   , _countOnes :: Int
                   } deriving (Show, Eq)

main :: IO ()
main = do
  exampleInput <- TIO.readFile "day10_example.txt"
  example2Input <- TIO.readFile "day10_example2.txt"
  puzzleInput  <- TIO.readFile "day10.txt"
  ts           <- testSpec "AdventOfCode" $ do
    it "works with example input" $ do
      let solution = solvePart1 . parseInput $ exampleInput
          expected = 35
      solution `shouldBe` expected
    it "works with puzzle input" $ do
      let solution = solvePart1 . parseInput $ puzzleInput
          expected = 2775
      solution `shouldBe` expected
    describe "Part 2" $ do
      it "works with example input" $ do
        let solution = solvePart2 . parseInput $ exampleInput
            expected = 8
        solution `shouldBe` expected
      it "works with example2 input" $ do
        let solution = solvePart2 . parseInput $ example2Input
            expected = 19208
        solution `shouldBe` expected
      it "works with puzzle input" $ do
        let solution = solvePart2 . parseInput $ puzzleInput
            expected = 518344341716992
        solution `shouldBe` expected
  defaultMain ts

solvePart1 :: [Adapter] -> Int
solvePart1 adapters = ones * threes
 where
  ones   = Map.findWithDefault 0 1 m
  threes = Map.findWithDefault 0 3 m
  m =
    Map.fromListWith (+)
      . map ((, 1 :: Int) . uncurry (subtract `on` adapterJolt))
      . pairs
      $ sort (extendAdapters adapters)

pairs :: [a] -> [(a,a)]
pairs xs = xs `zip` tail xs

extendAdapters :: [Adapter] -> [Adapter]
extendAdapters [] = []
extendAdapters adapters = Adapter 0 : maxAdapter adapters : adapters

maxAdapter :: [Adapter] -> Adapter
maxAdapter [] = Adapter 3
maxAdapter adapters = (\case (Adapter x) -> Adapter (x+3)) $ maximum adapters

solvePart2 :: [Adapter] -> Int
solvePart2 adapters =
  (Map.! Adapter 0) $
  foldl' step (Map.fromList [(maxAdapter adapters, 1)]) $ tail $
  reverse $
  map successors $
  filter (not . null) $
  tails (sort $ extendAdapters adapters)
  where successors [] = []
        successors (Adapter x:xs) = Adapter x : takeWhile (\case (Adapter x') -> abs (x' - x) < 4) xs
        step acc [] = acc
        step acc (k:succs) = Map.insert k result acc
          where result = foldl' (\acc' x -> acc' + acc Map.! x) 0 succs


parseInput :: Text -> [Adapter]
parseInput input = case Parsec.runParser parser () "stdin" input of
  Left  e -> error (show e)
  Right r -> r

parser :: Parsec Text () [Adapter]
parser =
  Parsec.many1
      (Adapter . read @Int <$> Parsec.many1 Parsec.digit <* Parsec.newline)
    <* Parsec.eof
