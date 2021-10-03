module Datum
  ( datum,
    Parser,
    Datum (..),
    dBool,
    dNumber,
    dChar,
    dString,
    dSymbol,
    show,
  )
where

import Control.Applicative (Alternative (many, (<|>)), empty, optional, some)
import Data.Char (GeneralCategory (Space), chr, generalCategory)
import Data.Functor (($>))
import My.Control.Monad.Trans.ParserT
import Numeric (readHex)

-- | A parser from `String` to values of type `a`.
type Parser a = ParserT Char Maybe a

-- | Parse a value wrapped in parentheses or square brackets.
paren :: Parser a -> Parser a
paren p = symbol "(" *> p <* symbol ")" <|> symbol "[" *> p <* symbol "]"

-- | Represents a Scheme data value.
data Datum
  = Bool Bool
  | Number Double
  | Char Char
  | String String
  | Symbol String
  | Cons Datum Datum
  | Empty
  deriving (Eq)

instance Show Datum where
  show (Cons (Symbol "quote") (Cons v Empty)) = "'" ++ show v
  show (Cons (Symbol "quasiquote") (Cons v Empty)) = "`" ++ show v
  show (Cons (Symbol "unquote") (Cons v Empty)) = "," ++ show v
  show (Cons (Symbol "unquote-splicing") (Cons v Empty)) = ",@" ++ show v
  show (Cons (Symbol "syntax") (Cons v Empty)) = "#'" ++ show v
  show (Cons (Symbol "quasisyntax") (Cons v Empty)) = "#`" ++ show v
  show (Cons (Symbol "unsyntax") (Cons v Empty)) = "#," ++ show v
  show (Cons (Symbol "unsyntax-splicing") (Cons v Empty)) = "#,@" ++ show v
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Number n) = show n
  show (Char '\x0007') = "#\\alarm"
  show (Char '\x0008') = "#\\backspace"
  show (Char '\x007f') = "#\\delete"
  show (Char '\x001b') = "#\\esc"
  show (Char '\x000a') = "#\\newline"
  show (Char '\x000c') = "#\\page"
  show (Char '\x000d') = "#\\return"
  show (Char ' ') = "#\\space"
  show (Char '\t') = "#\\tab"
  show (Char '\v') = "#\\vtab"
  show (Char c) = "#\\" ++ [c]
  show (String s) = show s
  show (Symbol s) = s
  show (Cons car cdr) = "(" ++ show car ++ expand cdr ++ ")"
    where
      expand :: Datum -> String
      expand Empty = ""
      expand (Cons car' cdr') = " " ++ show car' ++ expand cdr'
      expand d = " . " ++ show d
  show Empty = "()"

datum :: Parser Datum
datum =
  Bool <$> dBool
    <|> Char <$> dChar
    <|> Symbol <$> dSymbol
    <|> String <$> dString
    <|> Number <$> dNumber
    <|> dList

dBool :: Parser Bool
dBool = lexeme $ symbol "#t" $> True <|> symbol "#f" $> False

dChar :: Parser Char
dChar = lexeme $ literal "#\\" >> (characterName <|> item)

characterName :: Parser Char
characterName =
  literal "alarm" $> '\x0007'
    <|> literal "backspace" $> '\x0008'
    <|> literal "delete" $> '\x007f'
    <|> literal "esc" $> '\x001b'
    <|> (literal "linefeed" <|> literal "newline") $> '\x000a'
    <|> literal "page" $> '\x000c'
    <|> literal "return" $> '\x000d'
    <|> literal "space" $> ' '
    <|> literal "tab" $> '\t'
    <|> literal "vtab" $> '\v'

dSymbol :: Parser String
dSymbol =
  lexeme $
    (:) <$> initial <*> many subsequent
      <|> ((++) <$> literal "->" <*> many subsequent)
      <|> symbol "+"
      <|> symbol "-"
      <|> symbol "..."
  where
    initial = alpha <|> oneOf "!$%&*/:<=>?~_^"
    subsequent = initial <|> digit <|> oneOf ".+-@"

dString :: Parser String
dString = lexeme $ char '"' *> many strElem <* char '"'
  where
    strElem =
      (noneOf "\"\\" <|> escapeSequence <|> unicodeLiteral)
        <* optional escapeNewline

escapeSequence :: Parser Char
escapeSequence =
  literal "\\a" $> '\a'
    <|> literal "\\b" $> '\b'
    <|> literal "\\f" $> '\f'
    <|> literal "\\n" $> '\n'
    <|> literal "\\r" $> '\r'
    <|> literal "\\t" $> '\t'
    <|> literal "\\v" $> '\v'
    <|> literal "\\\"" $> '"'
    <|> literal "\\\\" $> '\\'

unicodeLiteral :: Parser Char
unicodeLiteral = do
  code <- literal "\\x" *> many digit16 <* literal ";"
  case readHex code of
    [(c, [])] -> return $ chr c
    _ -> empty
  where
    digit16 = digit <|> oneOf "abcdefABCDEF"

escapeNewline :: Parser String
escapeNewline =
  char '\\'
    *> many intralineWhitespace
    *> lineEnding <* many intralineWhitespace

lineEnding :: Parser String
lineEnding =
  literal "\n" -- newline
    <|> literal "\x0085" -- next-line
    <|> literal "\x2028" -- line-separator
    <|> literal "\r\n" -- carriage-return newline
    <|> literal "\r\x0085" -- carriage-return next-line
    <|> literal "\r" -- carriage-return

intralineWhitespace :: Parser Char
intralineWhitespace = match ((==) Space . generalCategory)

dNumber :: Parser Double
dNumber = lexeme $ do
  n <- read <$> some digit
  d <- fraction <|> return 0.0
  e <- numExponent <|> return 0
  return $ (n + d) * 10 ^^ e

fraction :: Parser Double
fraction =
  do
    d <- char '.' *> some digit
    return $ read $ '0' : '.' : d

numExponent :: Parser Int
numExponent = do
  _ <- char 'e' <|> char 'E'
  s <- optional (symbol "+" <|> symbol "-")
  n <- read <$> some digit
  return $ if s == Just "-" then - n else n

dList :: Parser Datum
dList =
  makeProper <$> paren (many datum)
    <|> paren
      ( do
          i <- some datum
          t <- symbol "." *> datum
          return $ makeImproper i t
      )
    <|> abbreviation

abbreviation :: Parser Datum
abbreviation = do
  s <-
    symbol "'" $> Symbol "quote"
      <|> symbol "`" $> Symbol "quasiquote"
      <|> symbol "," $> Symbol "unquote"
      <|> symbol ",@" $> Symbol "unquote-splicing"
      <|> symbol "#'" $> Symbol "syntax"
      <|> symbol "#`" $> Symbol "quasisyntax"
      <|> symbol "#," $> Symbol "unsyntax"
      <|> symbol "#,@" $> Symbol "unsyntax-splicing"
  d <- datum
  return $ makeProper [s, d]

makeProper :: [Datum] -> Datum
makeProper = foldr Cons Empty

makeImproper :: [Datum] -> Datum -> Datum
makeImproper [] _ = error "improper list cannot have less than 2 elements"
makeImproper [x] t = Cons x t
makeImproper (x : xs) t = Cons x (makeImproper xs t)
