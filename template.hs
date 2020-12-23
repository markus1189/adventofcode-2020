{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Applicative ((<|>))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  input <- TIO.getContents
  print  . parseInput $ input
  print . solvePart1 . parseInput $ input
  print . solvePart2 . parseInput $ input

solvePart1 :: _ -> _
solvePart1 _ = ()

solvePart2 :: _ -> _
solvePart2 _ = ()

parseInput :: Text -> _
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () _
parser = pure ()
