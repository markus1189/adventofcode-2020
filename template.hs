{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
import           Control.Applicative ((<|>))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

solvePart1 :: _ -> _
solvePart1 _ = ()

solvePart2 :: _ -> _
solvePart2 _ = ()

parseInput :: Text -> _
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec.Parsec Text () _
parser = pure ()
