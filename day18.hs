{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (Sum(..))
import Data.Functor.Identity
import           Control.Applicative ((<|>))
import           Text.Parsec (Parsec, (<?>))
import qualified Text.Parsec as Parsec
import           Text.Parsec.Expr (Assoc(..),Operator(..))
import qualified Text.Parsec.Expr as Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)

data Expr = Mult Expr Expr
          | Add Expr Expr
          | Lit Integer
          deriving (Show, Eq, Ord)

main :: IO ()
main = do
  input <- getContents
  print $ genericSolve . parseInput operatorTable1 $ input
  print $ genericSolve . parseInput operatorTable2 $ input -- 34646237037193

genericSolve :: [Expr] -> Integer
genericSolve = getSum . foldMap (Sum . eval)

eval :: Expr -> Integer
eval (Mult lhs rhs) = eval lhs * eval rhs
eval (Add lhs rhs) = eval lhs + eval rhs
eval (Lit n) = n

parseInput :: [[Operator String () Identity Expr]] -> String -> [Expr]
parseInput ot input =
  case Parsec.runParser (parser ot) () "stdin" input of
    Left  e -> error (show e)
    Right r -> r

parser :: [[Operator String a Identity Expr]] -> Parsec String a [Expr]
parser ot = Parsec.many1 (exprParser ot) <* Parsec.eof

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser haskellDef

exprParser :: [[Operator String a Identity Expr]] -> Parsec.ParsecT String a Identity Expr
exprParser ot = Parsec.buildExpressionParser ot (termParser ot) <?> "expression"

operatorTable1 :: [[Operator String a Identity Expr]]
operatorTable1 = [ [ Infix (Mult <$ Token.reservedOp lexer "*") AssocLeft
                  , Infix (Add <$ Token.reservedOp lexer "+") AssocLeft
                  ]
                ]

operatorTable2 :: [[Operator String a Identity Expr]]
operatorTable2 = [ [ Infix (Add <$ Token.reservedOp lexer "+") AssocLeft ]
                 , [ Infix (Mult <$ Token.reservedOp lexer "*") AssocLeft ]
                 ]


termParser :: [[Operator String a Identity Expr]] -> Parsec String a Expr
termParser ot = Token.parens lexer (exprParser ot) <|> Lit <$> Token.integer lexer
