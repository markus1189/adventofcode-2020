{-# LANGUAGE TypeApplications #-}

import           Control.Monad ( guard )
import           Data.Foldable ( for_ )
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec

data Solution = Solution1 Int Int
              | Solution2 Int Int Int
              deriving (Show, Eq)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser (numbers <* Parsec.eof) () "stdin" input of
    Left  e      -> error (show e)
    Right inputs -> do
      let result1 = take 1 $ handleInput1 inputs
          result2 = take 1 $ handleInput2 inputs
      for_ result1 $ \s -> putStrLn ("Result is: " <> showSolution s)
      for_ result2 $ \s -> putStrLn ("Result is: " <> showSolution s)

number :: Parsec.ParsecT T.Text u Identity Int
number = read @Int <$> Parsec.many1 Parsec.digit

numbers :: Parsec.ParsecT T.Text u Identity [Int]
numbers = Parsec.many1 (number <* Parsec.newline)

pick :: Int -> [a] -> [[a]]
pick 0 _  = return []
pick n xs = do
  r  <- xs
  rs <- pick (n - 1) xs
  return (r : rs)

handleInput1 :: [Int] -> [Solution]
handleInput1 xs = do
  [x1, x2] <- pick 2 xs
  guard $ x1 /= x2
  guard $ x1 + x2 == 2020
  return (Solution1 x1 x2)

handleInput2 :: [Int] -> [Solution]
handleInput2 xs = do
  [x1, x2, x3] <- pick 3 xs
  guard $ x1 + x2 + x3 == 2020
  return (Solution2 x1 x2 x3)

showSolution :: Solution -> String
showSolution (Solution1 x1 x2) =
  "x1=" <> show x1 <> ", x2=" <> show x2 <> ", solution=" <> show (x1 * x2)
showSolution (Solution2 x1 x2 x3) =
  "x1="
    <> show x1
    <> ", x2="
    <> show x2
    <> "x3="
    <> show x3
    <> ", solution="
    <> show (x1 * x2 * x3)
