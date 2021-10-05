module Datum
  ( datum,
    Parser,
    Constant (..),
    Datum (..),
    dBool,
    dNumber,
    dChar,
    dString,
    dSym,
    show,
  )
where

import Control.Applicative (Alternative (many, (<|>)), empty, optional, some)
import Control.Monad
import Data.Char (GeneralCategory (Space), chr, generalCategory, isAlpha, isDigit, isSpace)
import Data.Functor (($>))
import My.Control.Monad.Trans.ParserT
import Number
import Numeric (readDec, readHex, readInt, readOct)

-- | A parser from `String` to values of type `a`.
type Parser a = ParserT Char Maybe a

-- | Parse a continuous sequence of items not containing the given string.
commentText :: Parser ()
commentText =
  many (noneOf "#|")
    >> ( do
           i <- item
           unlike (if i == '#' then '|' else '#') >> commentText
       )
    <|> pure ()

-- | Make a parser consume any trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = many atmosphere *> p <* many atmosphere
  where
    atmosphere = void (match isSpace) <|> comment
    comment = lineComment <|> nestedComment <|> datumComment
    lineComment = like ';' >> many (match (/= '\n')) >> like '\n' $> ()
    nestedComment = string "#|" *> commentText <* commentCont <* string "|#"
    commentCont = many (nestedComment *> commentText)
    datumComment = string "#;" >> many atmosphere >> datum $> ()

-- | Parse exactly the given string and discard any trailing whitespace.
symbol :: String -> Parser String
symbol = lexeme . string

-- | Parse a value wrapped in parentheses or square brackets.
paren :: Parser a -> Parser a
paren p = symbol "(" *> p <* symbol ")" <|> symbol "[" *> p <* symbol "]"

-- | Represents any simple constant Scheme value.
data Constant
  = Bool Bool
  | Number Number
  | Char Char
  | String String
  | Sym String
  deriving (Eq)

instance Show Constant where
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
  show (Sym s) = s

-- | Represents a compound Scheme data value.
data Datum
  = Lexeme Constant
  | Pair Datum Datum
  | Empty
  deriving (Eq)

instance Show Datum where
  show (Lexeme c) = show c
  show (Pair (Lexeme (Sym "quote")) (Pair v Empty)) = "'" ++ show v
  show (Pair (Lexeme (Sym "quasiquote")) (Pair v Empty)) = "`" ++ show v
  show (Pair (Lexeme (Sym "unquote")) (Pair v Empty)) = "," ++ show v
  show (Pair (Lexeme (Sym "unquote-splicing")) (Pair v Empty)) = ",@" ++ show v
  show (Pair (Lexeme (Sym "syntax")) (Pair v Empty)) = "#'" ++ show v
  show (Pair (Lexeme (Sym "quasisyntax")) (Pair v Empty)) = "#`" ++ show v
  show (Pair (Lexeme (Sym "unsyntax")) (Pair v Empty)) = "#," ++ show v
  show (Pair (Lexeme (Sym "unsyntax-splicing")) (Pair v Empty)) =
    "#,@" ++ show v
  show (Pair car cdr) = "(" ++ show car ++ expand cdr ++ ")"
    where
      expand :: Datum -> String
      expand Empty = ""
      expand (Pair car' cdr') = " " ++ show car' ++ expand cdr'
      expand d = " . " ++ show d
  show Empty = "()"

datum :: Parser Datum
datum =
  Lexeme
    <$> lexeme
      ( Bool <$> dBool
          <|> Number <$> dNumber
          <|> Char <$> dChar
          <|> String <$> dString
          <|> Sym <$> dSym
      )
    <|> dList

dBool :: Parser Bool
dBool = string "#t" $> True <|> string "#f" $> False

dChar :: Parser Char
dChar = string "#\\" >> (characterName <|> item)

characterName :: Parser Char
characterName =
  string "alarm" $> '\x0007'
    <|> string "backspace" $> '\x0008'
    <|> string "delete" $> '\x007f'
    <|> string "esc" $> '\x001b'
    <|> (string "linefeed" <|> string "newline") $> '\x000a'
    <|> string "page" $> '\x000c'
    <|> string "return" $> '\x000d'
    <|> string "space" $> ' '
    <|> string "tab" $> '\t'
    <|> string "vtab" $> '\v'

dSym :: Parser String
dSym =
  (:) <$> initial <*> many subsequent
    <|> ((++) <$> string "->" <*> many subsequent)
    <|> symbol "+"
    <|> symbol "-"
    <|> symbol "..."
  where
    initial = match isAlpha <|> oneOf "!$%&*/:<=>?~_^"
    subsequent = initial <|> digit 10 <|> oneOf ".+-@"

dString :: Parser String
dString = like '"' *> many strElem <* like '"'
  where
    strElem =
      (noneOf "\"\\" <|> escapeSequence <|> unicodeLiteral)
        <* optional escapeNewline

escapeSequence :: Parser Char
escapeSequence =
  string "\\a" $> '\a'
    <|> string "\\b" $> '\b'
    <|> string "\\f" $> '\f'
    <|> string "\\n" $> '\n'
    <|> string "\\r" $> '\r'
    <|> string "\\t" $> '\t'
    <|> string "\\v" $> '\v'
    <|> string "\\\"" $> '"'
    <|> string "\\\\" $> '\\'

unicodeLiteral :: Parser Char
unicodeLiteral = do
  code <- string "\\x" *> many (digit 16) <* string ";"
  case readHex code of
    [(c, [])] -> return $ chr c
    _ -> empty

escapeNewline :: Parser String
escapeNewline =
  like '\\'
    *> many intralineWhitespace
    *> lineEnding <* many intralineWhitespace

lineEnding :: Parser String
lineEnding =
  string "\n" -- newline
    <|> string "\x0085" -- next-line
    <|> string "\x2028" -- line-separator
    <|> string "\r\n" -- carriage-return newline
    <|> string "\r\x0085" -- carriage-return next-line
    <|> string "\r" -- carriage-return

intralineWhitespace :: Parser Char
intralineWhitespace = match ((==) Space . generalCategory)

data Exactness = Exact | Inexact | Unspecified deriving (Eq, Show)

-- | Parser for any Scheme number literal.
dNumber :: Parser Number
dNumber = num 10 <|> num 16 <|> num 2 <|> num 8
  where
    num r = prefix r >>= complex r
    prefix r = radix r *> exactness <|> exactness <* radix r
    exactness =
      (like '#' >> (oneOf "iI" $> Datum.Inexact <|> oneOf "eE" $> Datum.Exact))
        <|> pure Unspecified

complex :: Int -> Exactness -> Parser Number
complex = real

real :: Int -> Exactness -> Parser Number
real r e =
  do
    s <- sign
    v <- ureal r e
    return $ if s == Minus then negate v else v
    <|> (like '+' >> naninf)
    <|> (like '-' >> negate <$> naninf)

naninf :: Parser Number
naninf = string "nan.0" $> nan <|> string "inf.0" $> inf
  where
    nan = 1 / 0
    inf = 0 / 0

data Sign = Plus | Minus deriving (Eq, Show)

sign :: Parser Sign
sign = like '-' $> Minus <|> optional (like '+') $> Plus

ureal :: Int -> Exactness -> Parser Number
ureal r e = ((/) <$> uint <*> (like '/' >> uint)) <|> decimal r e
  where
    uint = uinteger r e

decimal :: Int -> Exactness -> Parser Number
decimal 10 e@Datum.Exact = do
  i <- uinteger 10 e
  ex <- optional decimalExp
  case ex of
    Nothing -> return i
    Just ex' -> return $ i * 10 ^^ ex'
decimal 10 e =
  smallDecimal
    <|> bigDecimal
    <|> do
      i <- uinteger 10 e
      ex <- optional decimalExp
      case ex of
        Nothing -> return i
        Just ex' -> return $ blur i * 10 ^^ ex'
decimal _ _ = empty

smallDecimal :: Parser Number
smallDecimal = do
  _ <- like '.'
  ds <- some (digit 10)
  ex <- optional decimalExp
  let n = Number.Inexact (read $ "0." ++ ds :: Double)
  case ex of
    Nothing -> return n
    Just ex' -> return $ n * 10 ^^ ex'

bigDecimal :: Parser Number
bigDecimal = do
  ds <- some (digit 10)
  _ <- like '.'
  fs <- some (digit 10) <|> pure "0"
  ex <- optional decimalExp
  let n = Number.Inexact (read $ ds ++ "." ++ fs :: Double)
  case ex of
    Nothing -> return n
    Just ex' -> return $ n * 10 ^^ ex'

decimalExp :: Parser Integer
decimalExp = do
  s <- oneOf "eE" >> sign
  ex <- readBase 10 <$> some (digit 10)
  return $ case s of
    Plus -> ex
    Minus -> negate ex

uinteger :: Int -> Exactness -> Parser Number
uinteger r Datum.Inexact = Number.Inexact <$> (readBase r <$> some (digit r))
uinteger r _ = Number.Exact <$> (readBase r <$> some (digit r))

readBase :: (Eq a, Num a) => Int -> String -> a
readBase 2 s = v
  where
    [(v, "")] = readInt 2 (`elem` "01") (fromEnum . (== '1')) s
readBase 8 s = v where [(v, "")] = readOct s
readBase 10 s = v where [(v, "")] = readDec s
readBase 16 s = v where [(v, "")] = readHex s
readBase r _ = error $ "Attempt to read number in base " ++ show r

radix :: Int -> Parser ()
radix 2 = void $ like '#' >> oneOf "bB"
radix 8 = void $ like '#' >> oneOf "oO"
radix 10 = void $ optional (like '#' >> oneOf "dD")
radix 16 = void $ like '#' >> oneOf "xX"
radix _ = empty

digit :: Int -> Parser Char
digit 2 = oneOf "01"
digit 8 = oneOf "01234567"
digit 10 = match isDigit
digit 16 = match isDigit <|> oneOf "abcdefABCDEF"
digit _ = empty

-- | Parser for Scheme lists (both proper and improper).
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
  s <- Lexeme . Sym <$> abbrevPrefix
  d <- datum
  return $ makeProper [s, d]

abbrevPrefix :: Parser String
abbrevPrefix =
  string "'" $> "quote"
    <|> string "`" $> "quasiquote"
    <|> string "," $> "unquote"
    <|> string ",@" $> "unquote-splicing"
    <|> string "#'" $> "syntax"
    <|> string "#`" $> "quasisyntax"
    <|> string "#," $> "unsyntax"
    <|> string "#,@" $> "unsyntax-splicing"

makeProper :: [Datum] -> Datum
makeProper = foldr Pair Empty

makeImproper :: [Datum] -> Datum -> Datum
makeImproper [] _ = error "improper list cannot have less than 2 elements"
makeImproper [x] t = Pair x t
makeImproper (x : xs) t = Pair x (makeImproper xs t)
