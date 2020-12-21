{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Data.Foldable (foldl')
import           Data.List (sortBy, intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

newtype Ingredient = Ingredient { getIngredient :: String } deriving (Show, Eq, Ord)
newtype Allergen = Allergen String deriving (Show, Eq, Ord)

data Food = Food { foodIngredients :: [Ingredient]
                 , foodAllergens :: [Allergen]
                 } deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  putStrLn $ solvePart2 . parseInput $ input

solvePart1 :: [Food] -> Int
solvePart1 foods = foldl' (+) 0 $ mapMaybe (`Map.lookup` counts) ingredientsWithoutAllergens
  where
    ingredientsWithoutAllergens = Set.toList $ allIngredients `Set.difference` ingsWithAllergens
    allIngredients = foldMap (Set.fromList . foodIngredients) foods
    counts = wordCount foods
    ingsWithAllergens = foldl' Set.union Set.empty
                      . Map.elems
                      . buildAllergenMap
                      $ foods

solvePart2 :: [Food] -> String
solvePart2 foods = intercalate "," $ map (getIngredient . snd) $ sortBy (comparing fst) $ assign m
  where m = buildAllergenMap foods

buildAllergenMap :: Foldable f => f Food -> Map Allergen (Set Ingredient)
buildAllergenMap = Map.fromListWith Set.intersection
                 . concatMap distribute

assign :: Map Allergen (Set Ingredient) -> [(Allergen, Ingredient)]
assign m = if Map.null m
           then []
           else assignments ++ assign remaining'
  where (solutions, remaining) = Map.partition ((==1) . Set.size) m
        assignments = Map.toList $ fmap (head . Set.toList) solutions
        solvedIngredients = Set.fromList $ map snd assignments
        remaining' = (`Set.difference` solvedIngredients) <$> Map.withoutKeys remaining (Map.keysSet solutions)

wordCount :: [Food] -> Map Ingredient Int
wordCount fs = Map.fromListWith (+) $ concatMap (map (,1) . foodIngredients) fs

distribute :: Food -> [(Allergen, Set Ingredient)]
distribute f = map (,Set.fromList (foodIngredients f)) . foodAllergens $ f

parseInput :: Text -> [Food]
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () [Food]
parser = Parsec.many1 (Food <$> ingredientsParser <*> allergensParser <* Parsec.newline) <* Parsec.eof

wordParser :: Parsec Text () String
wordParser = Parsec.many1 Parsec.lower

ingredientsParser :: Parsec Text () [Ingredient]
ingredientsParser = Parsec.sepEndBy1 (Ingredient <$> wordParser) (Parsec.char ' ')

allergensParser :: Parsec Text () [Allergen]
allergensParser =
  (Parsec.between (Parsec.char '(') (Parsec.char ')') $
  Parsec.string "contains "
  *> Parsec.sepBy1 (Allergen <$> wordParser) (Parsec.string ", "))
