{-# LANGUAGE TypeApplications #-}
import           Control.Monad (replicateM, guard)
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
    Right r -> do
      print $ foldMap (\p -> if isValidPassport1 p then Sum @Int 1 else Sum 0) r
      print $ foldMap (\p -> if isValidPassport2 p then Sum @Int 1 else Sum 0) r

isValidPassport1 :: Map Key String -> Bool
isValidPassport1 m = all (`Map.member` m) requiredKeys

isValidPassport2 :: Map Key String -> Bool
isValidPassport2 m = all (\k -> k `Map.member` m && testValue k (m Map.! k) ) requiredKeys

testValue :: Key -> String -> Bool
testValue k v = case Parsec.runParser (valueParser k <* Parsec.eof) () "constant" v of
  Left _ -> False
  Right _ -> True

requiredKeys :: [Key]
requiredKeys = [ BirthYear
               , IssueYear
               , ExpirationYear
               , Height
               , HairColor
               , EyeColor
               , PassportID
               ]

valueParser :: Key -> Parsec.Parsec String () ()
valueParser BirthYear = parseNumber 4 >>= \x -> guard (x >= 1920 && x <= 2002)
valueParser IssueYear = parseNumber 4 >>= \x -> guard (x >= 2010 && x <= 2020)
valueParser ExpirationYear = parseNumber 4 >>= \x -> guard (x >= 2020 && x <= 2030)
valueParser Height = do
  n <- read @Int <$> Parsec.many1 Parsec.digit
  unit <- Left <$> Parsec.string "cm" <|> Right <$> Parsec.string "in"
  case unit of
    Left _ -> guard $ n >= 150 && n <= 193
    Right _ -> guard $ n >= 59 && n <= 76
valueParser HairColor = () <$ (Parsec.char '#' *> replicateM 6 (Parsec.digit <|> Parsec.oneOf ['a'..'f']))
valueParser EyeColor = do
  x <- (Parsec.choice . fmap (Parsec.try . Parsec.string) $ [ "amb"
                                                            , "blu"
                                                            , "brn"
                                                            , "gry"
                                                            , "grn"
                                                            , "hzl"
                                                            , "oth"
                                                            ])
  guard (not (null x))
valueParser PassportID = () <$ parseNumber 9
valueParser CountryID = return ()

parseNumber :: Int -> Parsec.Parsec String () Int
parseNumber n = read @Int <$> replicateM n Parsec.digit

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
