{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import Data.Maybe (listToMaybe, mapMaybe, catMaybes)
import Data.Foldable (for_)
import           Data.List (foldl', find)
import           Control.Applicative ((<|>))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)
import           Control.Monad (void)
import           Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Query.SP as SP
import Data.Graph.Inductive.PatriciaTree (Gr)

data ColoredBag = ColoredBag String String deriving (Show, Eq, Ord)

data Rule = Rule ColoredBag [(Int, ColoredBag)] deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser parser () "stdin" input of
    Left  e      -> error (show e)
    Right rules -> do
      let graph = buildGraph rules
          shinyGoldBag = find (\case (_,c) -> c == shinyGold) $ Graph.labNodes graph
      for_ shinyGoldBag $ \tgt -> do
        print (solvePart1 (fst tgt) graph) -- 164
        print (solvePart2 (fst tgt) graph - 1) -- 7872

solvePart1 :: Graph.Node -> Gr ColoredBag Int -> Int
solvePart1 shinyGoldBag graph =
  Set.size .
  Set.fromList .
  mapMaybe (Graph.lab graph) .
  mapMaybe listToMaybe .
  filter ((>1) . length) $
  catMaybes paths
  where paths = map (\node -> SP.sp node shinyGoldBag graph)
                    (Graph.nodes graph)

solvePart2 :: Graph.Node -> Gr ColoredBag Int -> Int
solvePart2 node graph = case Graph.match node graph of
  (Nothing, _) -> 0
  (Just ctx, graph') ->
    foldl' (+) 1 $
    map (\case (_,to,label) -> label * solvePart2 to graph')
        (Graph.out' ctx)

shinyGold :: ColoredBag
shinyGold = ColoredBag "shiny" "gold"

buildGraph :: [Rule] -> Gr ColoredBag Int
buildGraph rs = Graph.mkGraph nodesList (rs >>= mkEdges)
  where mkEdges (Rule k vs) = map (\case (n,v) -> (nodes ! k, nodes ! v, n)) vs
        nodes = Map.fromList $ map (\case (v,k) -> (k,v)) nodesList
        nodesList = [0..] `zip` map (\case (Rule k _) -> k) rs

parser :: Parsec Text () [Rule]
parser = Parsec.many1 lineParser <* Parsec.eof

lineParser :: Parsec Text () Rule
lineParser = ruleParser <* Parsec.newline

ruleParser :: Parsec Text () Rule
ruleParser = do
  bag <- bagParser
  void Parsec.space
  void $ Parsec.string "contain"
  void Parsec.space
  bags <- quantityBagParser `Parsec.sepBy1` Parsec.string ", " <|> [] <$ noOtherBags
  void $ Parsec.char '.'
  return (Rule bag bags)

quantityBagParser :: Parsec Text () (Int, ColoredBag)
quantityBagParser = do
  n <- read @Int <$> Parsec.many1 Parsec.digit
  void Parsec.space
  bag <- bagParser
  return (n,bag)

bagParser :: Parsec Text () ColoredBag
bagParser =
  colorParser <*
  Parsec.space <*
  Parsec.string "bag" <*
  Parsec.optional (Parsec.char 's')

colorParser :: Parsec Text () ColoredBag
colorParser =
  ColoredBag <$> Parsec.many1 Parsec.lower
             <*> (Parsec.space *> Parsec.many1 Parsec.lower)

noOtherBags :: Parsec Text () ()
noOtherBags = void $ Parsec.string "no other bags"
