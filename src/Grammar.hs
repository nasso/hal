module Grammar
  ( datum,
    Datum (..),
    dBoolean,
    dNumber,
    dCharacter,
    dString,
    dSymbol,
    show,
  )
where

import Control.Applicative (Alternative (many, (<|>)), empty, optional, some)
import Data.Char (GeneralCategory (Space), chr, generalCategory)
import Data.Functor (($>))
import My.Control.Monad.Trans
import Numeric (readHex)

-- | A parser from `String` to values of type `a`.
type Parser a = ParserT Char Maybe a

-- | Parse a value wrapped in parentheses or square brackets.
paren :: Parser a -> Parser a
paren p = symbol "(" *> p <* symbol ")" <|> symbol "[" *> p <* symbol "]"

-- | Represents a Scheme data value.
data Datum
  = Boolean Bool
  | Number Double
  | Character Char
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
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Number n) = show n
  show (Character '\x0007') = "#\\alarm"
  show (Character '\x0008') = "#\\backspace"
  show (Character '\x007f') = "#\\delete"
  show (Character '\x001b') = "#\\esc"
  show (Character '\x000a') = "#\\newline"
  show (Character '\x000c') = "#\\page"
  show (Character '\x000d') = "#\\return"
  show (Character ' ') = "#\\space"
  show (Character '\t') = "#\\tab"
  show (Character '\v') = "#\\vtab"
  show (Character c) = "#\\" ++ [c]
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
  Boolean <$> dBoolean
    <|> Character <$> dCharacter
    <|> Symbol <$> dSymbol
    <|> String <$> dString
    <|> Number <$> dNumber
    <|> dList

dBoolean :: Parser Bool
dBoolean = lexeme $ symbol "#t" $> True <|> symbol "#f" $> False

dCharacter :: Parser Char
dCharacter = lexeme $ literal "#\\" >> (characterName <|> next)

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
