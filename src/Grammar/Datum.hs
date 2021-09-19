module Grammar.Datum
  ( datum,
    Datum (..),
    dBoolean,
    dNumber,
    dCharacter,
    dString,
    dSymbol,
  )
where

import Control.Applicative (Alternative (many, (<|>)), empty, optional, some)
import Data.Char (GeneralCategory (Space), chr, generalCategory)
import Data.Functor (($>))
import Numeric (readHex)
import Parsing
  ( Parser,
    alpha,
    char,
    digit,
    getc,
    lexeme,
    literal,
    match,
    noneOf,
    oneOf,
    paren,
    symbol,
  )

-- | Represents a Scheme data value.
data Datum
  = Boolean Bool
  | Number Double
  | Character Char
  | String String
  | Symbol String
  | Cons Datum Datum
  | Empty
  deriving (Eq, Show)

datum :: Parser Datum
datum =
  Boolean <$> dBoolean
    <|> Character <$> dCharacter
    <|> Symbol <$> dSymbol
    <|> String <$> dString
    <|> Number <$> dNumber
    <|> dList

dBoolean :: Parser Bool
dBoolean = lexeme $ symbol "#t" $> True <|> symbol "#f" $> False

dCharacter :: Parser Char
dCharacter = lexeme $ literal "#\\" >> (characterName <|> getc)

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
      <|> symbol "+"
      <|> symbol "-"
      <|> symbol "..."
      <|> ((++) <$> literal "->" <*> many subsequent)
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
  char '\\'
    >> ( char 'a' $> '\a'
           <|> char 'b' $> '\b'
           <|> char 'f' $> '\f'
           <|> char 'n' $> '\n'
           <|> char 'r' $> '\r'
           <|> char 't' $> '\t'
           <|> char 'v' $> '\v'
           <|> char '"' $> '"'
           <|> char '\\' $> '\\'
       )

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
