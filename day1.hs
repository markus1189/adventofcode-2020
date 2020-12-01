{-# LANGUAGE TypeApplications #-}

import Data.List (nub, foldl')
import           Control.Monad ( guard , replicateM)
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import Control.Applicative (Alternative)

data Solution = Solution1 Int Int
              | Solution2 Int Int Int
              deriving (Show, Eq)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser (numbers <* Parsec.eof) () "stdin" input of
    Left  e      -> error (show e)
    Right inputs -> do
      let result1 = take 1 $ handleInput 2 inputs
          result2 = take 1 $ handleInput 3 inputs
      print (result1 ++ result2)

numbers :: Parsec.ParsecT T.Text u Identity [Int]
numbers = Parsec.many1 (read @Int <$> Parsec.many1 Parsec.digit <* Parsec.newline)

guards :: (Alternative m, Monad m) => [Int] -> m [Int]
guards xs = do
  guard $ nub xs == xs && foldl' (+) 0 xs == 2020
  return xs

handleInput :: Int -> [Int] -> [Int]
handleInput n xs = do
  rs <- replicateM n xs >>= guards
  return $ foldl' (*) 1 rs
