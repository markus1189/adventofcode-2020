{-# LANGUAGE TypeApplications #-}
import           Data.Monoid (Sum(..))
import           Control.Applicative ((<|>))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

data Key = BirthYear
         | IssueYear
         | ExpirationYear
         | Height
         | HairColor
         | EyeColor
         | PassportID
         | CountryID
         deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser parser () "stdin" input of
    Left  e      -> error (show e)
    Right r ->
      print $ foldMap (\p -> if isValidPassport p then Sum @Int 1 else Sum 0) r

isValidPassport :: Map Key String -> Bool
isValidPassport m = all (`Map.member` m) requiredKeys

requiredKeys :: [Key]
requiredKeys = [ BirthYear
               , IssueYear
               , ExpirationYear
               , Height
               , HairColor
               , EyeColor
               , PassportID
               ]

parser :: Parsec.Parsec Text () [Map Key String]
parser = parseBlock `Parsec.sepEndBy1` Parsec.newline <* Parsec.eof

parseBlock :: Parsec.Parsec Text () (Map Key String)
parseBlock = Map.fromList . concat <$> Parsec.many parseLine

parseLine :: Parsec.Parsec Text () [(Key, String)]
parseLine = (parseKv `Parsec.sepBy1` Parsec.char ' ') <* Parsec.newline

parseKv :: Parsec.Parsec Text () (Key, String)
parseKv = (,) <$> parseKey <* Parsec.char ':' <*> parseValue
  where parseValue = Parsec.many1 (Parsec.alphaNum <|> Parsec.char '#')

parseKey :: Parsec.Parsec Text () Key
parseKey =
  Parsec.choice .
  fmap Parsec.try $ [ BirthYear <$ Parsec.string "byr"
                    , IssueYear <$ Parsec.string "iyr"
                    , ExpirationYear <$ Parsec.string "eyr"
                    , Height <$ Parsec.string "hgt"
                    , HairColor <$ Parsec.string "hcl"
                    , EyeColor <$ Parsec.string "ecl"
                    , PassportID <$ Parsec.string "pid"
                    , CountryID <$ Parsec.string "cid"
                    ]
