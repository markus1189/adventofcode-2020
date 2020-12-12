{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
import           Control.Lens.Operators
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)
import           Data.List (foldl')
import           Control.Lens.TH

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

data Ship = Ship { _shipX :: Int, _shipY :: Int, _shipDirection :: Direction}
  deriving (Show, Eq)

makeLenses ''Ship

data Waypoint = Waypoint { _waypointX :: Int, _waypointY :: Int } deriving Show
makeLenses ''Waypoint

data ShipAndWaypoint = ShipAndWaypoint { _swShip :: Ship
                                       , _swWp :: Waypoint
                                       } deriving Show
makeLenses ''ShipAndWaypoint

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

solvePart1 :: [(Action, Int)] -> Int
solvePart1 = distance . foldl' step (Ship 0 0 East)
  where step :: Ship -> (Action, Int) -> Ship
        step s (RightTurn, i) = s & shipDirection %~ turnRight i
        step s (LeftTurn, i) = step s (RightTurn, negate i)
        step s@(Ship _ _ d) (Forward, i) = step s (Dir d,i)
        step s (Dir North, i) = s & shipY -~ i
        step s (Dir West,  i) = s & shipX -~ i
        step s (Dir South, i) = s & shipY +~ i
        step s (Dir East,  i) = s & shipX +~ i
        turnRight :: Int -> Direction -> Direction
        turnRight i d = (!! (i `mod` 360 `div` 90)) .  dropWhile (/=d) $ cycle [North .. West]

solvePart2 :: [(Action, Int)] -> Int
solvePart2 = distance . (^. swShip) . foldl' step (ShipAndWaypoint (Ship 0 0 East) (Waypoint 10 (-1)))
  where step :: ShipAndWaypoint -> (Action, Int) -> ShipAndWaypoint
        step sw (RightTurn, i) = sw & swWp %~ (!!n) . iterate rotRight
          where n = i `mod` 360 `div` 90
        step sw (LeftTurn, i) = step sw (RightTurn, (case i `mod` 360 `div` 90 of
                                                       1 -> 3*90
                                                       2 -> 2*90
                                                       3 -> 1*90
                                                       0 -> 0
                                                       _ -> error "oops"))
        step sw (Dir North, i) = sw & swWp . waypointY -~ i
        step sw (Dir South, i) = sw & swWp . waypointY +~ i
        step sw (Dir West,  i) = sw & swWp . waypointX -~ i
        step sw (Dir East,  i) = sw & swWp . waypointX +~ i
        step sw (Forward, i) = iterate moveWaypoint sw !! i
        moveWaypoint :: ShipAndWaypoint -> ShipAndWaypoint
        moveWaypoint sw = sw & swShip . shipX +~ (sw ^. swWp . waypointX) & swShip . shipY +~ (sw ^. swWp . waypointY)

distance :: Ship -> Int
distance (Ship x y _) = abs x + abs y

rotRight :: Waypoint -> Waypoint
rotRight (Waypoint dx dy) = Waypoint (negate dy) dx

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
