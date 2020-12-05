{-# LANGUAGE TypeApplications #-}
import           Control.Applicative ((<|>))
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as Parsec
import           Data.Text (Text)

main :: IO ()
main = do
  input <- TIO.getContents
  case Parsec.runParser parser () "stdin" input of
    Left  e      -> error (show e)
    Right r -> do
      print r

parser :: Parsec.Parsec Text () _
parser = return ()
