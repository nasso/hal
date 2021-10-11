module Syntax
  ( Program (..),
    Form (..),
    Expression (..),
    Formals (..),
    Var,
    AstReader,
    readProgram,
  )
where

import Control.Monad (join)
import Control.Monad.Trans.Parser
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Stream (ListStream (ListStream))
import Datum (Datum)
import qualified Datum

type Var = String

newtype Program = Program [Form] deriving (Eq, Show)

data Form = Def [(Var, Expression)] | Expr Expression deriving (Eq, Show)

data Expression
  = Lit Datum.Constant
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

type AstReader a = ParserT (ListStream Datum) Identity a

exec' :: [Datum] -> AstReader a -> AstReader (a, ListStream Datum)
exec' l = exec (ListStream l 0)

constant :: AstReader Datum.Constant
constant = do
  Datum.Lexeme c <- item
  return c

var :: AstReader Var
var = do
  Datum.Sym s <- constant
  return s

sym :: String -> AstReader Datum
sym = like . Datum.Lexeme . Datum.Sym

-- | Parse a proper list, and run a parser on its elements.
properP :: AstReader a -> AstReader a
properP p = do
  Datum.List l <- item
  fst <$> exec' l (p <* eof)

-- | Parse an improper list, run one parser on its elements and another on its
-- tail.
improperP :: AstReader a -> AstReader b -> AstReader (a, b)
improperP pa pb = do
  Datum.ImproperList l l' <- item
  a <- fst <$> exec' (NonEmpty.toList l) (pa <* eof)
  b <- fst <$> exec' [Datum.Lexeme l'] (pb <* eof)
  return (a, b)

readProgram :: AstReader Program
readProgram = Program <$> many form

form :: AstReader Form
form = Def <$> definition <|> Expr <$> expression

definition :: AstReader [(Var, Expression)]
definition =
  join
    <$> some
      ( properP
          ( (: []) <$> ((,) <$> (sym "define" *> var) <*> expression)
              <|> join <$> (sym "begin" >> many definition)
          )
      )

expression :: AstReader Expression
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

formals :: AstReader Formals
formals =
  Variadic [] <$> var
    <|> Strict <$> properP (many var)
    <|> uncurry Variadic <$> improperP (many var) var
