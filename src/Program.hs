module Program
  ( Program (..),
    Form (..),
    Definition (..),
    Expression (..),
    Constant (..),
    Formals (..),
    Var,
    Parser,
    program,
    form,
  )
where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Datum (Datum (..))
import My.Control.Monad.Trans.ParserT

type Var = String

newtype Program = Program [Form] deriving (Eq, Show)

data Form
  = Def Definition
  | Expr Expression
  deriving (Eq, Show)

data Definition
  = VarDef Var Expression
  | Begin [Definition]
  deriving (Eq, Show)

data Expression
  = Lit Constant
  | Sym Var
  | Quote Datum
  | Lambda Formals (NonEmpty Expression)
  | If Expression Expression Expression
  | Set Var Expression
  | Application Expression [Expression]
  deriving (Eq, Show)

data Constant
  = Bool Bool
  | Number Double
  | Char Char
  | String String
  deriving (Eq, Show)

data Formals
  = Exact [Var]
  | Variadic [Var] Var
  deriving (Eq, Show)

type Parser a = ParserT Datum Maybe a

var :: Parser Var
var = do
  Symbol s <- item
  return s

sym :: String -> Parser Datum
sym = like . Symbol

-- | Parse a proper list.
proper :: Parser [Datum]
proper = do
  v <- item
  case v of
    Cons car cdr -> (car :) . fst <$> exec [cdr] proper
    Empty -> return []
    _ -> empty

-- | Parse an improper list.
improper :: Parser (NonEmpty Datum, Datum)
improper = do
  Cons car cdr <- item
  case cdr of
    cdr'@(Cons _ _) -> do
      (car', l) <- fst <$> exec [cdr'] improper
      return (car <| car', l)
    cdr' -> return (car :| [], cdr')

-- | Parse a proper list, and run a parser on its elements.
properP :: Parser a -> Parser a
properP p = do
  l <- proper
  fst <$> exec l (p <* eof)

-- | Parse an improper list, run one parser on its elements and another on its
-- tail.
improperP :: Parser a -> Parser b -> Parser (a, b)
improperP pa pb = do
  (l, l') <- improper
  a <- fst <$> exec (NonEmpty.toList l) (pa <* eof)
  b <- fst <$> exec [l'] (pb <* eof)
  return (a, b)

program :: Parser Program
program = Program <$> many form

form :: Parser Form
form = Def <$> definition <|> Expr <$> expression

definition :: Parser Definition
definition =
  properP $
    VarDef <$> (sym "define" *> var) <*> expression
      <|> Begin <$> (sym "begin" *> many definition)

expression :: Parser Expression
expression =
  Lit <$> constant
    <|> Sym <$> var
    <|> properP
      ( Quote <$> (sym "quote" *> item)
          <|> Lambda <$> (sym "lambda" *> formals) <*> NonEmpty.some1 expression
          <|> If <$> (sym "if" *> expression) <*> expression <*> expression
          <|> Set <$> (sym "set!" *> var) <*> expression
          <|> Application <$> expression <*> many expression
      )

constant :: Parser Constant
constant =
  do Datum.Bool b <- item; return $ Program.Bool b
    <|> do Datum.Number n <- item; return $ Program.Number n
    <|> do Datum.Char c <- item; return $ Program.Char c
    <|> do Datum.String s <- item; return $ Program.String s

formals :: Parser Formals
formals =
  Variadic [] <$> var
    <|> Exact <$> properP (many var)
    <|> uncurry Variadic <$> improperP (many var) var
