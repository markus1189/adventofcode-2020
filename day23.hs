{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Monad (void)
import           Data.Array (Array)
import qualified Data.Array as Array
import           Data.Foldable (foldlM)
import           Data.Functor (($>))
import           Data.IORef (IORef, newIORef, atomicWriteIORef, readIORef)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Cup = Cup { cupLabel :: Int
               , cupNext :: IORef Cup
               }

mkCup :: Int -> IO Cup
mkCup lbl = Cup lbl <$> newIORef (error "unset next pointer")

buildCups :: Traversable f => f Int -> IO (f Cup)
buildCups = traverse mkCup

linkCups :: NonEmpty Cup -> IO ()
linkCups (c :| cs) = do
  endOfLinkedCups <- foldlM link c cs
  atomicWriteIORef (cupNext endOfLinkedCups) c

link :: Cup -> Cup -> IO Cup
link acc cup = atomicWriteIORef (cupNext acc) cup $> cup

cupTake :: Int -> Cup -> IO [Int]
cupTake 0 _ = pure []
cupTake n c = do
  next <- nextCup c
  (cupLabel c :) <$> cupTake (n-1) next

nextCup :: Cup -> IO Cup
nextCup = readIORef . cupNext

main :: IO ()
main = do
  input <- TIO.getContents
  (solvePart1 . parseInput $ input) >>= putStrLn
  (solvePart2 . parseInput $ input) >>= print

solvePart1 :: NonEmpty Int -> IO String
solvePart1 ds = do
  cups <- buildCups ds
  linkCups cups
  let ary = Array.array (1, maxLabel) (map (\c -> (cupLabel c, c)) (NonEmpty.toList cups))
      maxLabel = maximum ds
  genericSolve (NonEmpty.head cups) ary maxLabel 100
  concatMap show . drop 1 <$> cupTake maxLabel (ary Array.! 1)

solvePart2 :: NonEmpty Int -> IO Int
solvePart2 prefix = do
  let ds = prefix <> NonEmpty.fromList [10..oneMillion]
      repetitions = 10 * 1000 * 1000
      oneMillion = 1 * 1000 * 1000
  cups <- buildCups ds
  linkCups cups
  let ary = Array.array (1, maxLabel) (map (\c -> (cupLabel c, c)) (NonEmpty.toList cups))
      maxLabel = oneMillion
      firstCup = ary Array.! 1
  genericSolve (NonEmpty.head cups) ary maxLabel repetitions
  c1 <- nextCup firstCup
  c2 <- nextCup c1
  pure (cupLabel c1 * cupLabel c2)

genericSolve :: Cup -> Array Int Cup -> Int -> Int -> IO ()
genericSolve s ary maxLabel reps = do
  void $ foldlM (\acc _ -> makeMove ary maxLabel acc) s [1..reps]

nextCandidate :: Int -> Int -> Int
nextCandidate maxLabel label = if label - 1 == 0
                               then maxLabel
                               else label - 1


calculateDestinationCup :: Int -> [Int] -> Int -> Int
calculateDestinationCup maxLabel removedLabels currentLabel =
  head $ dropWhile (`elem` removedLabels) . drop 1 $ iterate (nextCandidate maxLabel) currentLabel

makeMove :: Array Int Cup -> Int -> Cup -> IO Cup
makeMove ary maxLabel cup = do
  segment <- cut 3 cup
  xs <- segmentToList segment
  let destLabel = calculateDestinationCup maxLabel xs (cupLabel cup)
      destinationCup = ary Array.! destLabel
  paste destinationCup segment
  nextCup cup

segmentToList :: (Cup, Cup) -> IO [Int]
segmentToList (start, end) = do
  if cupLabel start == cupLabel end
  then pure [cupLabel end]
  else do
    next <- nextCup start
    (cupLabel start :) <$> segmentToList (next, end)

cut :: Int -> Cup -> IO (Cup, Cup)
cut n cup = do
  lastOfSegment <- foldlM (\cup' _ -> nextCup cup') cup (replicate n ())
  headOfSegment <- nextCup cup
  nextCup lastOfSegment  >>= atomicWriteIORef (cupNext cup)
  pure (headOfSegment, lastOfSegment)

paste :: Cup -> (Cup, Cup) -> IO ()
paste targetCup (headOfSegment, lastOfSegment) = do
  next <- nextCup targetCup
  atomicWriteIORef (cupNext targetCup) headOfSegment
  atomicWriteIORef (cupNext lastOfSegment) next

parseInput :: Text -> NonEmpty Int
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () (NonEmpty Int)
parser = NonEmpty.fromList . map (read @Int . pure) <$> Parsec.many1 Parsec.digit <* Parsec.newline <* Parsec.eof
