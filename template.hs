{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
import           Control.Applicative ((<|>))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Text.Parsec (Parsec)
import           Data.Text (Text)

import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  ts <- testSpec "AdventOfCode" $ do
    it "works on stdin" $ do
      input <- TIO.getContents
      let solution = solve . parseInput $ input
          expected = ()
      solution `shouldBe` expected
  defaultMain ts

solve :: _ -> _
solve _ = ()

parseInput :: Text -> _
parseInput input =
  case Parsec.runParser parser () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: Parsec.Parsec Text () _
parser = pure ()
