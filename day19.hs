{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Foldable (foldl')
import           Data.List (unfoldr)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Numeric.Natural
import           Text.Parsec (Parsec, (<?>))
import qualified Text.Parsec as Parsec

newtype RuleId = RuleId Natural deriving (Show, Eq, Ord, Num)

data RuleExpr = RuleLookup RuleId
              | RuleChar Char
              | RuleOr RuleExpr RuleExpr
              | RuleAnd RuleExpr RuleExpr
              deriving (Eq, Ord)

ruleAll1 :: [RuleExpr] -> RuleExpr
ruleAll1 [] = error "ruleAll1 called on empty list"
ruleAll1 (r:rs) = foldl' RuleAnd r rs

ruleAny1 :: [RuleExpr] -> RuleExpr
ruleAny1 [] = error "ruleAny1 called on empty list"
ruleAny1 (r:rs) = foldl' RuleOr r rs

instance Show RuleExpr where
  show (RuleLookup (RuleId rid)) = "lookup(" <> show rid <> ")"
  show (RuleChar c) = show c
  show (RuleOr lhs rhs) = "(" <> show lhs <> ") | (" <> show rhs <> ")"
  show (RuleAnd lhs rhs) = "(" <> show lhs <> ") & (" <> show rhs <> ")"

toParser :: Map RuleId RuleExpr -> RuleExpr -> Parsec Text () ()
toParser m (RuleLookup ruleId) = toParser m (m Map.! ruleId)
toParser _ (RuleChar c) = void $ Parsec.char c
toParser m (RuleOr expr1 expr2) = Parsec.try (toParser m expr1) <|> toParser m expr2
toParser m (RuleAnd expr1 expr2) = toParser m expr1 *> toParser m expr2

data Input = Input { _inputRules :: Map RuleId RuleExpr
                   , _inputStrings :: [String]
                   } deriving (Show, Eq, Ord)

check :: Parsec Text () () -> (Int, String) -> Bool
check p (lineNumber, input) = case Parsec.runParser p () ("input " <> show lineNumber) (Text.pack input) of
  Left _ -> False
  Right _ -> True

main :: IO ()
main = do
  input <- TIO.getContents
  print $ solvePart1 . parseInput $ input
  print $ solvePart2 . parseInput $ input

solvePart1 :: Input -> Int
solvePart1 (Input rules strings) = length $ filter (check p) (zip @Int [1..] strings)
  where p = toParser rules r0 <* Parsec.eof
        r0 = rules Map.! 0

solvePart2 :: Input -> Int
solvePart2 (Input originalRules strings) = length $ filter (check p) (zip @Int [1..] strings)
  where p = toParser rules' r0 <* Parsec.eof
        r0 = buildRule8And11 (maximum $ map length strings)
        rules' = Map.delete 8 . Map.delete 11 $ originalRules

unfoldRule8 :: [RuleExpr]
unfoldRule8 = unfoldr (\s -> Just (s, RuleAnd s (RuleLookup 42))) (RuleLookup 42)

unfoldRule11 :: [RuleExpr]
unfoldRule11 = unfoldr (\case (pref,suf) -> Just (ruleAll1 (pref ++ suf), (RuleLookup 42 : pref, RuleLookup 31 : suf))) ([RuleLookup 42], [RuleLookup 31])

buildRule8And11 :: Int -> RuleExpr
buildRule8And11 maxLength = ruleAny1 combinations
  where combinations = do
          r8 <- take (maxLength - 2) unfoldRule8
          r11 <- take ((maxLength - 1) `div` 2) unfoldRule11
          pure (RuleAnd r8 r11)

parseInput :: Text -> Input
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec Text () Input
parser = Input <$> (Map.fromList <$> Parsec.many1 (ruleExprParser <* Parsec.newline))
               <* Parsec.newline
               <*> Parsec.many1 (Parsec.many1 (Parsec.oneOf "ab") <* Parsec.newline) <* Parsec.eof

numParser :: forall n. (Read n, Num n) => Parsec Text () n
numParser = read @n <$> Parsec.many1 Parsec.digit

ruleIdParser :: Parsec Text () RuleId
ruleIdParser = RuleId <$> numParser <?> "ruleId"

ruleExprParser :: Parsec Text () (RuleId, RuleExpr)
ruleExprParser = do
  ruleId <- ruleIdParser
  void $ Parsec.string ": "
  (ruleId,) <$> ruleBodyParser

ruleBodyParser :: Parsec Text () RuleExpr
ruleBodyParser = ruleCharParser <|> concatParser
  where
    concatParser = do
      ruleIds <- ruleConcatParser
      optionalOr <- Parsec.optionMaybe $ Parsec.string "| " *> ruleConcatParser
      case optionalOr of
        Nothing -> pure ruleIds
        Just other -> pure $ RuleOr ruleIds other

ruleLookupParser :: Parsec Text () RuleExpr
ruleLookupParser = RuleLookup . RuleId <$> numParser @Natural <?> "ruleLookup"

ruleCharParser :: Parsec Text () RuleExpr
ruleCharParser = RuleChar <$> (Parsec.char '"' *> Parsec.anyChar <* Parsec.char '"') <?> "ruleChar"

ruleConcatParser :: Parsec Text () RuleExpr
ruleConcatParser = combine <$> Parsec.sepEndBy1 ruleLookupParser (Parsec.char ' ') <?> "ruleConcat"
  where combine (x:xs) = foldl' RuleAnd x xs
        combine _ = error "can't happen with sepEndBy1"


-- data RuleExpr = RuleLookup RuleId
--               | RuleChar Char
--               | RuleOr RuleExpr RuleExpr
--               deriving (Show, Eq, Ord)
