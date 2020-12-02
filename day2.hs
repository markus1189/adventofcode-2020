{-# LANGUAGE TypeApplications #-}

import Data.Bits (xor)
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec

data Solution = Solution1 Int Int
              | Solution2 Int Int Int
              deriving (Show, Eq)

data Constraint = Constraint Int Int Char deriving (Show, Eq)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser (parseLine `Parsec.sepEndBy` (void Parsec.newline <|> void Parsec.eof)) () "stdin" input of
    Left  e      -> do
      putStrLn "Ooops, something went wrong."
      error (show e)
    Right inputs -> do
      putStrLn $ "Result for 1 is: " <> show (length (filter (uncurry checkConstraint1) inputs))
      putStrLn $ "Result for 2 is: " <> show (length (filter (uncurry checkConstraint2) inputs))

parseLine :: Parsec.ParsecT T.Text u Identity (Constraint, String)
parseLine = do
  lower <- read @Int <$> Parsec.many1 Parsec.digit
  _ <- Parsec.char '-'
  upper <- read @Int <$> Parsec.many1 Parsec.digit
  _ <- Parsec.space
  c <- Parsec.anyChar
  _ <- Parsec.char ':' <* Parsec.space
  s <- Parsec.many1 Parsec.alphaNum
  return (Constraint lower upper c, s)

checkConstraint1 :: Constraint -> String -> Bool
checkConstraint1 (Constraint l u c) input = result
  where filteredInput = filter (== c) input
        count = length filteredInput
        result = count >= l && count <= u

checkConstraint2 :: Constraint -> String -> Bool
checkConstraint2 (Constraint start end c) input =
  -- TODO: ugly and slow :shrug:
  (input !! (start-1) == c) `xor` (input !! (end-1) == c)
