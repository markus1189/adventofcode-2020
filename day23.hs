{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Monad (void)
import           Data.Array.MArray
import           Data.Array.IO (IOArray)
import           Data.Foldable (foldlM, foldrM)
import           Data.Maybe (fromMaybe)
import           Data.STM.LinkedList (LinkedList, Node)
import qualified Data.STM.LinkedList as LinkedList
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           GHC.Conc.Sync (atomically)
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- TIO.getContents
  (solvePart1 . parseInput $ input) >>= putStrLn
  (solvePart2 . parseInput $ input) >>= print

solvePart1 :: [Int] -> IO String
solvePart1 ds = do
  ll <- makeList ds
  _ <- genericSolve ll (maximum ds) 100
  atomically $ concatMap show . take (length ds - 1) . drop 1 . dropWhile (/=1) . cycle <$> LinkedList.toList ll

solvePart2 :: [Int] -> IO Int
solvePart2 prefix = do
  let ds = prefix ++ [10..oneMillion]
      repetitions = 10 * 1000 * 1000
      oneMillion = 1 * 1000 * 1000
  ll <- makeList ds
  ary <- genericSolve ll oneMillion repetitions
  firstCup <- readArray ary 1
  n1 <- moveNext ll firstCup
  n2 <- moveNext ll n1
  pure (LinkedList.value n1 * LinkedList.value n2)

genericSolve :: LinkedList Int -> Int -> Int -> IO (IOArray Int (Node Int))
genericSolve ll maxLabel reps = do
  s <- unsafeStart ll
  ary <- newArray_ @IOArray (1,maxLabel)
  buildArray s ary
  _ <- foldlM (\acc _ -> makeMove ary maxLabel ll acc) s [1..reps]
  pure ary

buildArray :: Node Int -> IOArray Int (Node Int) -> IO ()
buildArray n array = do
  writeArray array (LinkedList.value n) n
  nextNode <- atomically $ LinkedList.next n
  case nextNode of
    Nothing -> pure ()
    Just n' -> buildArray n' array

nextCandidate :: Int -> Int -> Int
nextCandidate maxLabel label = if label - 1 == 0
                               then maxLabel
                               else label - 1


calculateDestinationCup :: Int -> [Int] -> Int -> Int
calculateDestinationCup maxLabel removedLabels currentLabel =
  head $ dropWhile (`elem` removedLabels) . drop 1 $ iterate (nextCandidate maxLabel) currentLabel

makeMove :: IOArray Int (Node Int) -> Int -> LinkedList Int -> Node Int -> IO (Node Int)
makeMove ary maxLabel ll node = do
  xs <- cut 3 ll node
  let destLabel = calculateDestinationCup maxLabel xs (LinkedList.value node)
  destinationCup <- readArray ary destLabel
  paste ary xs destinationCup
  n <- moveNext ll node
  pure n

cut :: Int -> LinkedList a -> Node a -> IO [a]
cut n ll node = foldlM (\acc _ -> (:acc) <$> removeNext ll node) [] (replicate n ())

paste :: IOArray Int (Node Int) -> [Int] -> Node Int -> IO ()
paste ary xs n = void $ foldrM (\x curNode -> do
                                      nextNode <- atomically (LinkedList.insertAfter x curNode)
                                      _ <- writeArray ary x nextNode
                                      pure nextNode
                                  ) n xs

removeNext :: LinkedList a -> Node a -> IO a
removeNext ll n = do
  nextNode <- moveNext ll n
  atomically $ LinkedList.delete nextNode
  pure (LinkedList.value nextNode)

unsafeStart :: LinkedList a -> IO (Node a)
unsafeStart = atomically . fmap (fromMaybe (error "LinkedList was empty")) . LinkedList.start

moveNext :: LinkedList a -> Node a -> IO (Node a)
moveNext ll n = do
  next <- atomically $ LinkedList.next n
  case next of
    Nothing -> unsafeStart ll
    Just nextNode -> pure nextNode

makeList :: [Int] -> IO (LinkedList Int)
makeList [] = atomically LinkedList.empty
makeList (d:ds) = do
  ll <- LinkedList.emptyIO
  node <- atomically $ LinkedList.append d ll
  _ <- foldlM (\acc x -> atomically (LinkedList.insertAfter x acc)) node ds
  pure ll

parseInput :: Text -> [Int]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Int]
parser = map (read @Int . pure) <$> Parsec.many1 Parsec.digit <* Parsec.newline <* Parsec.eof
