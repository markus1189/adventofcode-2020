{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Parallel.Strategies (parMap, rseq)
import Data.STRef
import Control.Monad.ST
import           Control.Monad (void)
import           Data.Array (Array)
import qualified Data.Array as Array
import           Data.Foldable (foldlM)
import           Data.Functor (($>))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Cup s = Cup { cupLabel :: Int
                 , cupNext :: STRef s (Cup s)
                 }

mkCup :: Int -> ST s (Cup s)
mkCup lbl = Cup lbl <$> newSTRef (error "unset next pointer")

buildCups :: Traversable f => f Int -> ST s (f (Cup s))
buildCups = traverse mkCup

linkCups :: NonEmpty (Cup s) -> ST s ()
linkCups (c :| cs) = do
  endOfLinkedCups <- foldlM link c cs
  writeSTRef (cupNext endOfLinkedCups) c

link :: Cup s -> Cup s -> ST s (Cup s)
link acc cup = writeSTRef (cupNext acc) cup $> cup

cupTake :: Int -> Cup s -> ST s [Int]
cupTake 0 _ = pure []
cupTake n c = do
  next <- nextCup c
  (cupLabel c :) <$> cupTake (n-1) next

nextCup :: Cup s -> ST s (Cup s)
nextCup = readSTRef . cupNext

main :: IO ()
main = do
  input <- TIO.getContents
  putStrLn $ runST (solvePart1 . parseInput $ input)
  print $ runST (solvePart2 . parseInput $ input)

solvePart1 :: NonEmpty Int -> ST s String
solvePart1 ds = do
  cups <- buildCups ds
  linkCups cups
  let ary = Array.array (1, maxLabel) (map (\c -> (cupLabel c, c)) (NonEmpty.toList cups))
      maxLabel = maximum ds
  genericSolve (NonEmpty.head cups) ary maxLabel 100
  concatMap show . drop 1 <$> cupTake maxLabel (ary Array.! 1)

solvePart2 :: NonEmpty Int -> ST s Int
solvePart2 prefix = do
  let ds = prefix <> NonEmpty.fromList [10..oneMillion]
      repetitions = 10 * 1000 * 1000
      oneMillion = 1 * 1000 * 1000
  cups <- buildCups ds
  linkCups cups
  let ary = Array.array (0, maxLabel+10) (map (\c -> (cupLabel c, c)) (NonEmpty.toList cups))
      maxLabel = oneMillion
      firstCup = ary Array.! 1
  genericSolve (NonEmpty.head cups) ary maxLabel repetitions
  c1 <- nextCup firstCup
  c2 <- nextCup c1
  pure (cupLabel c1 * cupLabel c2)

genericSolve :: Cup s -> Array Int (Cup s) -> Int -> Int -> ST s ()
genericSolve s ary maxLabel reps = do
  void $ foldlM (\acc _ -> makeMove ary maxLabel acc) s [1..reps]

nextCandidate :: Int -> Int -> Int
nextCandidate maxLabel label = if label - 1 == 0
                               then maxLabel
                               else label - 1


calculateDestinationCup :: Int -> [Int] -> Int -> Int
calculateDestinationCup maxLabel removedLabels currentLabel =
  head $ dropWhile (`elem` removedLabels) . drop 1 $ iterate (nextCandidate maxLabel) currentLabel

makeMove :: Array Int (Cup s) -> Int -> Cup s -> ST s (Cup s)
makeMove ary maxLabel cup = do
  segment <- cut 3 cup
  xs <- segmentToList segment
  let destLabel = calculateDestinationCup maxLabel xs (cupLabel cup)
      destinationCup = ary Array.! destLabel
  paste destinationCup segment
  nextCup cup

segmentToList :: (Cup s, Cup s) -> ST s [Int]
segmentToList (start, end) = do
  if cupLabel start == cupLabel end
  then pure [cupLabel end]
  else do
    next <- nextCup start
    (cupLabel start :) <$> segmentToList (next, end)

cut :: Int -> Cup s -> ST s (Cup s, Cup s)
cut n cup = do
  lastOfSegment <- foldlM (\cup' _ -> nextCup cup') cup (replicate n ())
  headOfSegment <- nextCup cup
  nextCup lastOfSegment  >>= writeSTRef (cupNext cup)
  pure (headOfSegment, lastOfSegment)

paste :: Cup s -> (Cup s, Cup s) -> ST s ()
paste targetCup (headOfSegment, lastOfSegment) = do
  next <- nextCup targetCup
  writeSTRef (cupNext targetCup) headOfSegment
  writeSTRef (cupNext lastOfSegment) next

parseInput :: Text -> NonEmpty Int
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () (NonEmpty Int)
parser = NonEmpty.fromList . map (read @Int . pure) <$> Parsec.many1 Parsec.digit <* Parsec.newline <* Parsec.eof
