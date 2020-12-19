{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Applicative ((<|>))
import           Control.Lens (view,_2,to)
import           Data.List (foldl', transpose, isPrefixOf, partition)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

newtype Ticket = Ticket { getTicket :: [Int] } deriving (Show, Eq)

data Interval = Interval { _intervalMin :: Int
                         , _intervalMax :: Int
                         } deriving (Show, Eq, Ord)

data Constraint = Constraint { _cName :: String
                             , _cFirstInterval :: Interval
                             , _cSecondInterval :: Interval
                             } deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input -- 23054
  print $ solvePart2 . parseInput $ input -- 51240700105297

solvePart1 :: ([Constraint], Ticket, [Ticket]) -> Int
solvePart1 (cs, _, ts) = foldl' (+) 0 $ concatMap (invalidNumbers cs) ts

solvePart2 :: ([Constraint], Ticket, [Ticket]) -> Int
solvePart2 (cs, Ticket myTicket, ts) =
  foldl' (\acc i -> acc * myTicket !! i) 1
  . map fst
  . filter (("departure" `isPrefixOf`) . view (_2 . to _cName))
  $ orderedConstraints
  where validTickets = filter (checkTicket cs) ts
        orderedConstraints = solve $ zip @Int [0..] $ figureOutConstraints cs validTickets

solve :: [(Int, [Constraint])] -> [(Int, Constraint)]
solve [] = []
solve ccs = let ([(i, [c])],rest) = partition ((==1) . length . snd) ccs
            in (i,c) : solve (map (\case (j,cs) -> (j, filter (/=c) cs)) rest)


invalidNumbers :: [Constraint] -> Ticket -> [Int]
invalidNumbers cs (Ticket ts) = filter (not . validForAnyConstraint cs) ts

validForAnyConstraint :: [Constraint] -> Int -> Bool
validForAnyConstraint cs i = any (checkConstraint i) cs

checkConstraint :: Int -> Constraint -> Bool
checkConstraint i (Constraint _ (Interval firstLower firstUpper) (Interval secondLower secondUpper)) =
  i >= firstLower && i <= firstUpper || i >= secondLower && i <= secondUpper

checkTicket :: [Constraint] -> Ticket -> Bool
checkTicket cs (Ticket ts) = all (validForAnyConstraint cs) ts

figureOutConstraints :: [Constraint] -> [Ticket] -> [[Constraint]]
figureOutConstraints cs validTickets = map (\column -> filter (\c -> all (`checkConstraint` c) column) cs) columns
  where columns = transpose $ map getTicket validTickets

parseInput :: Text -> ([Constraint], Ticket, [Ticket])
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () ([Constraint], Ticket, [Ticket])
parser = (,,) <$> (constraintsParser <* Parsec.newline)
              <*> (yourTicketParser <* Parsec.newline)
              <*> (nearbyTicketParser <* Parsec.eof)

constraintsParser :: Parsec Text () [Constraint]
constraintsParser = Parsec.many1 (constraintParser <* Parsec.newline)

yourTicketParser :: Parsec Text () Ticket
yourTicketParser = Parsec.string "your ticket:" <* Parsec.newline
                *> ticketParser <* Parsec.newline

ticketParser :: Parsec Text () Ticket
ticketParser = Ticket <$> Parsec.sepBy numberParser (Parsec.char ',')

nearbyTicketParser :: Parsec Text () [Ticket]
nearbyTicketParser = Parsec.string "nearby tickets:"
                  *> Parsec.many1 (ticketParser <* Parsec.newline)

constraintParser :: Parsec Text () Constraint
constraintParser = Constraint
               <$> (name <* Parsec.string ": ")
               <*> intervalParser <* Parsec.string " or "
               <*> intervalParser
  where name = Parsec.many1 (Parsec.lower <|> Parsec.char ' ')

intervalParser :: Parsec Text () Interval
intervalParser = Interval <$> numberParser <* Parsec.char '-' <*> numberParser

numberParser :: Parsec Text () Int
numberParser = read <$> Parsec.many1 Parsec.digit
