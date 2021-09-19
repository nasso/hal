module Grammar.Program
  ( Program,
    Form (..),
    Definition (..),
    Expression (..),
    Constant (..),
    Formals (..),
    program,
    form,
  )
where

import Control.Applicative (Alternative (many, some, (<|>)))
import Grammar.Datum
  ( Datum,
    dBoolean,
    dCharacter,
    dNumber,
    dString,
    dSymbol,
    datum,
  )
import Parsing (Parser, paren, symbol)

type Program = [Form]

data Form = Def Definition | Expr Expression deriving (Show, Eq)

data Definition
  = VarDef String Expression
  | Begin [Definition]
  deriving (Show, Eq)

data Expression
  = Constant Constant
  | Var String
  | Quote Datum
  | Lambda Formals Expression [Expression]
  | If Expression Expression Expression
  | Set String Expression
  | Application Expression [Expression]
  deriving (Show, Eq)

data Constant
  = Boolean Bool
  | Number Double
  | Char Char
  | String String
  deriving (Show, Eq)

data Formals
  = Single String
  | Exact [String]
  | Variable [String] String
  deriving (Show, Eq)

program :: Parser Program
program = many form

form :: Parser Form
form = Def <$> definition <|> Expr <$> expr

definition :: Parser Definition
definition =
  paren $
    VarDef <$> (symbol "define" *> dSymbol) <*> expr
      <|> Begin <$> (symbol "begin" *> many definition)

expr :: Parser Expression
expr =
  Constant <$> constant
    <|> Var <$> dSymbol
    <|> paren
      ( Quote <$> (symbol "quote" *> datum)
          <|> Lambda <$> (symbol "lambda" *> formals) <*> expr <*> many expr
          <|> If <$> (symbol "if" *> expr) <*> expr <*> expr
          <|> Set <$> (symbol "set!" *> dSymbol) <*> expr
          <|> Application <$> expr <*> many expr
      )

formals :: Parser Formals
formals =
  Single <$> dSymbol
    <|> paren
      ( Exact <$> many dSymbol
          <|> Variable <$> some dSymbol <*> (symbol "." *> dSymbol)
      )

constant :: Parser Constant
constant =
  Boolean <$> dBoolean
    <|> Number <$> dNumber
    <|> Char <$> dCharacter
    <|> String <$> dString
