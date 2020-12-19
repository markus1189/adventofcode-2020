import           Data.List (foldl')
import           Data.Monoid (Sum(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

newtype Answers = Answers [Char] deriving (Show, Eq, Ord)
newtype FlightGroup = FlightGroup [Answers] deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser parser () "stdin" input of
    Left  e   -> error (show e)
    Right fgs -> do
      print $ solvePart1 fgs
      print $ solvePart2 fgs

solvePart1 :: [FlightGroup] -> Int
solvePart1 = getSum . foldMap (Sum . distinctAnswers) -- 6565

solvePart2 :: [FlightGroup] -> Int
solvePart2 = getSum . foldMap (Sum . questionsAllAnswered) -- 3137

questionsAllAnswered :: FlightGroup -> Int
questionsAllAnswered (FlightGroup as) =
  Set.size $ foldl' Set.intersection (Set.fromList possibleAnswers) $ map answersToSet as

distinctAnswers :: FlightGroup -> Int
distinctAnswers (FlightGroup as) =
  Set.size $ foldl' Set.union Set.empty $ map answersToSet as

answersToSet  :: Answers -> Set Char
answersToSet (Answers vs) = Set.fromList vs

parser :: Parsec.Parsec Text () [FlightGroup]
parser = groupParser `Parsec.sepEndBy1` Parsec.newline

answerParser :: Parsec Text () Char
answerParser = Parsec.oneOf possibleAnswers

lineParser :: Parsec Text () Answers
lineParser = Answers <$> Parsec.many1 answerParser <* Parsec.newline

groupParser :: Parsec Text () FlightGroup
groupParser = FlightGroup <$> Parsec.many1 lineParser

possibleAnswers :: [Char]
possibleAnswers = ['a'..'z']
