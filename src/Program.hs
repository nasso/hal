module Program
  ( Program (..),
    Form (..),
    Expression (..),
    Constant (..),
    Formals (..),
    Var,
    Parser,
    program,
    form,
  )
where

import Control.Monad
import Control.Monad.Trans.Parser
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Stream (ListStream (..))
import Datum (Constant (..), Datum (..))

type Var = String

newtype Program = Program [Form] deriving (Eq, Show)

data Form = Def [(Var, Expression)] | Expr Expression deriving (Eq, Show)

data Expression
  = Lit Constant
  | Quote Datum
  | Begin (NonEmpty Expression)
  | Lambda Formals (NonEmpty Expression)
  | If Expression Expression Expression
  | Set Var Expression
  | Application Expression [Expression]
  deriving (Eq, Show)

data Formals
  = Strict [Var]
  | Variadic [Var] Var
  deriving (Eq, Show)

type Parser a = ParserT (ListStream Datum) Identity a

exec' :: [Datum] -> Parser a -> Parser (a, ListStream Datum)
exec' l = exec (ListStream l 0)

constant :: Parser Constant
constant = do
  Lexeme c <- item
  return c

var :: Parser Var
var = do
  Sym s <- constant
  return s

sym :: String -> Parser Datum
sym = like . Lexeme . Sym

-- | Parse a proper list, and run a parser on its elements.
properP :: Parser a -> Parser a
properP p = do
  List l <- item
  fst <$> exec' l (p <* eof)

-- | Parse an improper list, run one parser on its elements and another on its
-- tail.
improperP :: Parser a -> Parser b -> Parser (a, b)
improperP pa pb = do
  ImproperList l l' <- item
  a <- fst <$> exec' (NonEmpty.toList l) (pa <* eof)
  b <- fst <$> exec' [Lexeme l'] (pb <* eof)
  return (a, b)

program :: Parser Program
program = Program <$> many form

form :: Parser Form
form = Def <$> definition <|> Expr <$> expression

definition :: Parser [(Var, Expression)]
definition =
  join
    <$> some
      ( properP
          ( (: []) <$> ((,) <$> (sym "define" *> var) <*> expression)
              <|> join <$> (sym "begin" >> many definition)
          )
      )

expression :: Parser Expression
expression =
  Lit <$> constant
    <|> properP
      ( Quote <$> (sym "quote" *> item)
          <|> Begin <$> (sym "begin" *> some1 expression)
          <|> Lambda <$> (sym "lambda" *> formals) <*> some1 expression
          <|> If <$> (sym "if" *> expression) <*> expression <*> expression
          <|> Set <$> (sym "set!" *> var) <*> expression
          <|> Application <$> expression <*> many expression
      )

formals :: Parser Formals
formals =
  Variadic [] <$> var
    <|> Strict <$> properP (many var)
    <|> uncurry Variadic <$> improperP (many var) var
