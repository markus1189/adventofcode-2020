{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Foldable (for_)
import           Data.List (tails, genericLength, genericTake, find, inits)
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad (guard)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser (parser @Int) () "stdin" input of
    Left  e      -> error (show e)
    Right numbers -> do
      for_ (solvePart1 @Int 25 numbers) $ \r1 -> do
        print r1 -- 85848519
        print $ solvePart2 r1 numbers -- 13414198

solvePart1 :: (Integral n, Num a, Ord a) => n -> [a] -> Maybe a
solvePart1 n is =
  fmap last $
  find (\w -> not $ last w `Set.member` pairSet (init w)) $
  slidingWindow (n+1) is

solvePart2 :: (Num a, Ord a) => a -> [a] -> Maybe a
solvePart2 x is =
  fmap ((+) <$> maximum <*> minimum) $
  find ((==x) . sum) $ concatMap inits $ tails is

parser :: forall n. Read n => Parsec.Parsec Text () [n]
parser = Parsec.many1 (number <* Parsec.newline) <* Parsec.eof
  where number = read @n <$> Parsec.many1 Parsec.digit

slidingWindow :: Integral n => n -> [a] -> [[a]]
slidingWindow n = filter ((==n) . genericLength) . map (genericTake n) . tails

pairSet :: (Ord a, Num a) => [a] -> Set a
pairSet xs = Set.fromList $ do
  x <- xs
  y <- xs
  guard $ x /= y
  pure (x+y)
