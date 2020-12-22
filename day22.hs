{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Lens (preview, _Cons, to)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Foldable (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

newtype Card = Card Int deriving (Show, Eq, Ord)
makePrisms ''Card

newtype Deck = Deck [Card] deriving (Show, Eq, Ord)
makePrisms ''Deck

data Player = Player { _playerNumber :: Int
                     , _playerDeck :: Deck
                     } deriving (Show, Eq, Ord)
makeLenses ''Player

data TurnResult = Win Player
                | Continue (Player, Player)
                deriving (Show, Eq, Ord)

topCard :: Player -> Maybe (Card, Player)
topCard p = maybeHeadTail <&> \case (c,cs) -> (c, p & playerDeck . _Deck .~ cs)
  where maybeHeadTail = preview (playerDeck . _Deck . _Cons) p

putAtBottom :: [Card] -> Player -> Player
putAtBottom cards p = p & playerDeck . _Deck <>~ cards

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

solvePart1 :: (Player, Player) -> Int
solvePart1 = score . play 1

playTurn :: (Player, Player) -> TurnResult
playTurn (p1, p2) = case (,) <$> getTopCard p1 p2 <*> getTopCard p2 p1 of
  Left w -> Win w
  Right config -> Continue $ winByHighCard config

winByHighCard :: ((Card, Player), (Card, Player)) -> (Player, Player)
winByHighCard ((cardP1, restP1), (cardP2, restP2)) =
  if cardP1 >= cardP2
  then (putAtBottom [cardP1, cardP2] restP1, restP2)
  else (restP1, putAtBottom [cardP2, cardP1] restP2)

getTopCard :: Player -> Player -> Either Player (Card, Player)
getTopCard p otherP = maybe (Left otherP) Right (topCard p)

play :: Int -> (Player, Player) -> Player
play n ps = case playTurn ps of
  Win p -> p
  Continue ps' -> play (n+1) ps'

score :: Player -> Int
score p = foldl' (+) 0 $ zipWith (*) (reverse (p ^.. playerDeck . _Deck . traverse . _Card)) [1..]

solvePart2 :: (Player, Player) -> Int
solvePart2 ps = score $ playRecursiveCombat Set.empty (1,1) ps

setupRecursiveConfig :: ((Card, Player), (Card, Player)) -> (Player, Player)
setupRecursiveConfig ((Card p1v, p1), (Card p2v, p2)) =
  (p1 & playerDeck . _Deck %~ take p1v, p2 & playerDeck . _Deck %~ take p2v)

playRecursiveCombat :: Set (Player, Player) -> (Int, Int) -> (Player, Player) -> Player
playRecursiveCombat prevs (gameNr,roundNr) cur@(p1, p2) =
  if cur `Set.member` prevs then p1 else
  case (,) <$> getTopCard p1 p2 <*> getTopCard p2 p1 of
  Left p -> p
  Right config@((cardP1, restP1), (cardP2, restP2)) ->
     if not (canRecurse config)
     then playRecursiveCombat (Set.insert cur prevs) (gameNr, roundNr + 1) $ winByHighCard config
     else let (Player num _) = playRecursiveCombat Set.empty (gameNr+1,1) (setupRecursiveConfig config)
              nextConfig = if num == p1 ^. playerNumber
                           then let restP1' = putAtBottom [cardP1, cardP2] restP1 in (restP1', restP2)
                           else let restP2' = putAtBottom [cardP2, cardP1] restP2 in (restP1, restP2')
          in playRecursiveCombat (Set.insert cur prevs) (gameNr, roundNr+1) nextConfig

canRecurse :: ((Card, Player), (Card, Player)) -> Bool
canRecurse ((Card p1v, p1), (Card p2v, p2)) =
  p1v <= p1 ^. playerDeck . _Deck . to length && p2v <= p2 ^. playerDeck . _Deck . to length

parseInput :: Text -> (Player, Player)
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () (Player, Player)
parser = (,) <$> playerParser <* Parsec.newline <*> playerParser <* Parsec.eof

playerParser :: Parsec Text () Player
playerParser = Player <$> n <*> deckParser
  where n = Parsec.string "Player "
         *> (read @Int <$> Parsec.many1 Parsec.digit)
         <* Parsec.char ':'
          <* Parsec.newline

deckParser :: Parsec Text () Deck
deckParser = Deck <$> Parsec.many1 (cardParser <* Parsec.newline)

cardParser :: Parsec Text () Card
cardParser = Card <$> read @Int <$> Parsec.many1 Parsec.digit
