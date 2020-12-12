{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)
import           Data.List (foldl')

data Action = Dir Direction
            | LeftTurn
            | RightTurn
            | Forward
            deriving (Show, Eq)

data Direction = North
               | East
               | South
               | West
               deriving (Show, Eq, Enum)

data S = S (Int, Int) Direction
  deriving (Show, Eq)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

solvePart1 :: [(Action, Int)] -> Int
solvePart1 = (\case (S (x,y) _) -> abs x + abs y) . foldl' step (S (0,0) East)
  where step :: S -> (Action, Int) -> S
        step (S pos d) (RightTurn, i) = S pos (turnRight d i)
        step (S pos d) (LeftTurn, i) = S pos (turnRight d (negate i))
        step s@(S _ d) (Forward, i) = step s (Dir d,i)
        step (S (x,y) d) (Dir North, i) = S (x,  y-i) d
        step (S (x,y) d) (Dir West,  i) = S (x-i,  y) d
        step (S (x,y) d) (Dir South, i) = S (x,  y+i) d
        step (S (x,y) d) (Dir East,  i) = S (x+i,  y) d

turnRight :: Direction -> Int -> Direction
turnRight d i = head . drop (i `mod` 360 `div` 90) .  dropWhile (/=d) $ cycle [North .. West]

solvePart2 :: [(Action, Int)] -> _
solvePart2 _ = ()

parseInput :: Text -> [(Action, Int)]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [(Action, Int)]
parser = Parsec.many1 (actionParser <* Parsec.newline) <* Parsec.eof

actionParser :: Parsec Text () (Action, Int)
actionParser = (,) <$> p <*> (read @Int <$> Parsec.many1 Parsec.digit)
  where p = Parsec.choice [ Dir North <$ Parsec.char 'N'
                          , Dir South <$ Parsec.char 'S'
                          , Dir East <$ Parsec.char 'E'
                          , Dir West <$ Parsec.char 'W'
                          , LeftTurn <$ Parsec.char 'L'
                          , RightTurn <$ Parsec.char 'R'
                          , Forward <$ Parsec.char 'F'
                          ]
