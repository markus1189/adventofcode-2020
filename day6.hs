import           Data.Monoid (Sum(..))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)
import           Data.Set (Set)
import qualified Data.Set as Set

newtype Answers = Answers [Char] deriving (Show, Eq, Ord)
newtype FlightGroup = FlightGroup [Answers] deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser parser () "stdin" input of
    Left  e      -> error (show e)
    Right fgs -> do
      print $ solvePart1 fgs

solvePart1 :: [FlightGroup] -> Int
solvePart1 = getSum . foldMap (Sum . distinctAnswers) -- 6565

distinctAnswers :: FlightGroup -> Int
distinctAnswers (FlightGroup as) = Set.size $ foldMap toSet as
  where toSet :: Answers -> Set Char
        toSet (Answers vs) = Set.fromList vs

parser :: Parsec.Parsec Text () [FlightGroup]
parser = groupParser `Parsec.sepEndBy1` Parsec.newline

answerParser :: Parsec Text () Char
answerParser = Parsec.oneOf ['a'..'z']

lineParser :: Parsec Text () Answers
lineParser = Answers <$> Parsec.many1 answerParser <* Parsec.newline

groupParser :: Parsec Text () FlightGroup
groupParser = FlightGroup <$> Parsec.many1 lineParser
