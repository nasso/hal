{-# LANGUAGE TypeFamilies #-}

module Datum
  ( datum,
    Constant (..),
    Datum (..),
    makeImproper,
    dBool,
    dNumber,
    dChar,
    dString,
    dSym,
    show,
  )
where

import Control.Monad
import Control.Monad.Parser
import Data.Char
  ( GeneralCategory (..),
    chr,
    generalCategory,
    isAlpha,
    isDigit,
    isSpace,
    ord,
  )
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Number
import Numeric (readDec, readHex, readInt, readOct)

-- | Parse a continuous sequence of items not containing the given string.
commentText :: CharParser p => p ()
commentText = void $ many $ notFollowedBy (string "#|" <|> string "|#") >> item

-- | Whitespace as defined by the R6RS standard.
whitespace :: CharParser p => p ()
whitespace = void $ match isSpace <?> "whitespace"

-- | Make a parser consume any trailing whitespace.
lexeme :: CharParser p => p a -> p a
lexeme p = many atmosphere *> p <* many atmosphere
  where
    atmosphere = whitespace <|> comment
    comment = lineComment <|> nestedComment <|> datumComment <?> "comment"
    lineComment = void $ like ';' >> many (match (/= '\n')) >> like '\n'
    nestedComment = string "#|" *> commentText <* commentCont <* string "|#"
    commentCont = many (nestedComment *> commentText)
    datumComment = void $ string "#;" >> many atmosphere >> datum

-- | Parse exactly the given string and discard any trailing whitespace.
symbol :: CharParser p => String -> p String
symbol s = lexeme (string s) <?> "\"" ++ s ++ "\""

-- | Parse a value wrapped in parentheses or square brackets.
paren :: CharParser p => p a -> p a
paren p = symbol "(" *> p <* symbol ")" <|> symbol "[" *> p <* symbol "]"

-- | A delimiter for identifiers, ., numbers, characters and booleans.
delimiter :: CharParser p => p ()
delimiter = void (oneOf "()[]\";#") <|> whitespace

-- | The '.' symbol.
dot :: CharParser p => p ()
dot = like '.' >> followedBy (delimiter <|> eof)

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
  | List [Datum]
  | ImproperList (NonEmpty Datum) Constant
  deriving (Eq)

instance Show Datum where
  show (Lexeme c) = show c
  show (List [Lexeme (Sym "quote"), v]) = "'" ++ show v
  show (List [Lexeme (Sym "quasiquote"), v]) = "`" ++ show v
  show (List [Lexeme (Sym "unquote"), v]) = "," ++ show v
  show (List [Lexeme (Sym "unquote-splicing"), v]) = ",@" ++ show v
  show (List [Lexeme (Sym "syntax"), v]) = "#'" ++ show v
  show (List [Lexeme (Sym "quasisyntax"), v]) = "#`" ++ show v
  show (List [Lexeme (Sym "unsyntax"), v]) = "#," ++ show v
  show (List [Lexeme (Sym "unsyntax-splicing"), v]) = "#,@" ++ show v
  show (List elems) = "(" ++ unwords (map show elems) ++ ")"
  show (ImproperList elems tail') =
    "(" ++ unwords (map show $ NonEmpty.toList elems)
      ++ " . "
      ++ show tail'
      ++ ")"

datum :: CharParser p => p Datum
datum = Lexeme <$> lexeme literal <|> dList
  where
    literal =
      Bool <$> dBool
        <|> Number <$> dNumber
        <|> Char <$> dChar
        <|> String <$> dString
        <|> Sym <$> dSym
        <?> "literal"

dBool :: CharParser p => p Bool
dBool =
  string "#t" $> True <|> string "#f" $> False
    <* followedBy (delimiter <|> eof)
    <?> "boolean literal"

dChar :: CharParser p => p Char
dChar =
  string "#\\" >> (characterName <|> item)
    <* followedBy (delimiter <|> eof)
    <?> "character literal"

characterName :: CharParser p => p Char
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

inlineHexEscape :: CharParser p => p Char
inlineHexEscape = do
  code <- string "\\x" *> many (digit 16) <* string ";"
  case readHex code of
    [(c, [])] -> return $ chr c
    _ -> expected "hexadecimal value"

isConstituent :: Char -> Bool
isConstituent c | isAlpha c = True
isConstituent c | ord c > 127 = isConstituentCategory $ generalCategory c
isConstituent _ = False

isConstituentCategory :: GeneralCategory -> Bool
isConstituentCategory UppercaseLetter = True
isConstituentCategory LowercaseLetter = True
isConstituentCategory TitlecaseLetter = True
isConstituentCategory ModifierLetter = True
isConstituentCategory OtherLetter = True
isConstituentCategory NonSpacingMark = True
isConstituentCategory LetterNumber = True
isConstituentCategory OtherNumber = True
isConstituentCategory DashPunctuation = True
isConstituentCategory ConnectorPunctuation = True
isConstituentCategory OtherPunctuation = True
isConstituentCategory CurrencySymbol = True
isConstituentCategory MathSymbol = True
isConstituentCategory ModifierSymbol = True
isConstituentCategory OtherSymbol = True
isConstituentCategory PrivateUse = True
isConstituentCategory _ = False

dSym :: CharParser p => p String
dSym =
  let init' = match isConstituent <|> oneOf "!$%&*/:<=>?^_~" <|> inlineHexEscape
      subsequent = init' <|> digit 10 <|> oneOf ".+-@"
   in ( (:) <$> init' <*> many subsequent
          <|> ((++) <$> string "->" <*> many subsequent)
          <|> string "+"
          <|> string "-"
          <|> string "..."
      )
        <* followedBy (delimiter <|> eof)
        <?> "identifier"

dString :: CharParser p => p String
dString = like '"' *> many strElem <* like '"' <?> "string literal"
  where
    strElem =
      (noneOf "\"\\" <|> escapeSequence <|> inlineHexEscape)
        <* optional escapeNewline

escapeSequence :: CharParser p => p Char
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
    <?> "escape sequence"

escapeNewline :: CharParser p => p ()
escapeNewline =
  like '\\'
    *> many intralineWhitespace
    *> lineEnding <* many intralineWhitespace

lineEnding :: CharParser p => p ()
lineEnding =
  void $
    string "\n" -- newline
      <|> string "\x0085" -- next-line
      <|> string "\x2028" -- line-separator
      <|> string "\r\n" -- carriage-return newline
      <|> string "\r\x0085" -- carriage-return next-line
      <|> string "\r" -- carriage-return

intralineWhitespace :: CharParser p => p Char
intralineWhitespace = match ((==) Space . generalCategory)

data Exactness = Exact | Inexact | Unspecified deriving (Eq, Show)

-- | Parser for any Scheme number literal.
dNumber :: CharParser p => p Number
dNumber =
  choice [num 10, num 16, num 2, num 8] <* followedBy (delimiter <|> eof)
    <?> "number literal"
  where
    num r = prefix r >>= complex r
    prefix r = radix r *> exactness <|> exactness <* radix r
    exactness =
      (like '#' >> (oneOf "iI" $> Datum.Inexact <|> oneOf "eE" $> Datum.Exact))
        <|> pure Unspecified

complex :: CharParser p => Int -> Exactness -> p Number
complex = real

real :: CharParser p => Int -> Exactness -> p Number
real r e =
  do
    s <- sign
    v <- ureal r e
    return $ if s == Minus then negate v else v
    <|> (like '+' >> naninf)
    <|> (like '-' >> negate <$> naninf)

naninf :: CharParser p => p Number
naninf = string "nan.0" $> nan <|> string "inf.0" $> inf
  where
    nan = 1 / 0
    inf = 0 / 0

data Sign = Plus | Minus deriving (Eq, Show)

sign :: CharParser p => p Sign
sign = like '-' $> Minus <|> optional (like '+') $> Plus

ureal :: CharParser p => Int -> Exactness -> p Number
ureal r e = ((/) <$> uint <*> (like '/' >> nonzero)) <|> decimal r e
  where
    uint = uinteger r e <?> "natural"
    nonzero = satisfy uint (/= 0) <?> "non-zero value"

decimal :: CharParser p => Int -> Exactness -> p Number
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
decimal r _ = error $ "decimal called with r = " ++ show r

smallDecimal :: CharParser p => p Number
smallDecimal = do
  _ <- like '.'
  ds <- many1 (digit 10)
  ex <- optional decimalExp
  let n = Number.Inexact (read $ "0." ++ ds :: Double)
  case ex of
    Nothing -> return n
    Just ex' -> return $ n * 10 ^^ ex'

bigDecimal :: CharParser p => p Number
bigDecimal = do
  ds <- many1 (digit 10)
  _ <- like '.'
  fs <- many1 (digit 10) <|> pure "0"
  ex <- optional decimalExp
  let n = Number.Inexact (read $ ds ++ "." ++ fs :: Double)
  case ex of
    Nothing -> return n
    Just ex' -> return $ n * 10 ^^ ex'

decimalExp :: CharParser p => p Integer
decimalExp = do
  s <- oneOf "eE" >> sign
  ex <- readBase 10 <$> many1 (digit 10)
  return $ case s of
    Plus -> ex
    Minus -> negate ex

uinteger :: CharParser p => Int -> Exactness -> p Number
uinteger r Datum.Inexact = Number.Inexact <$> (readBase r <$> many1 (digit r))
uinteger r _ = Number.Exact <$> (readBase r <$> many1 (digit r))

readBase :: (Eq a, Num a) => Int -> String -> a
readBase 2 s = v
  where
    [(v, "")] = readInt 2 (`elem` "01") (fromEnum . (== '1')) s
readBase 8 s = v where [(v, "")] = readOct s
readBase 10 s = v where [(v, "")] = readDec s
readBase 16 s = v where [(v, "")] = readHex s
readBase r _ = error $ "Attempt to read number in base " ++ show r

radix :: CharParser p => Int -> p ()
radix 2 = void $ like '#' >> oneOf "bB"
radix 8 = void $ like '#' >> oneOf "oO"
radix 10 = void $ optional (like '#' >> oneOf "dD")
radix 16 = void $ like '#' >> oneOf "xX"
radix r = error $ "unsupported radix: " ++ show r

digit :: CharParser p => Int -> p Char
digit 2 = oneOf "01" <?> "binary digit"
digit 8 = oneOf "01234567" <?> "octal digit"
digit 10 = match isDigit <?> "decimal digit"
digit 16 = match isDigit <|> oneOf "abcdefABCDEF" <?> "hexadecimal digit"
digit r = error $ "unsupported radix: " ++ show r

-- | Parser for Scheme lists (both proper and improper).
dList :: CharParser p => p Datum
dList =
  paren
    ( do
        ds <- many datum
        case ds of
          [] -> return $ List []
          (d : ds') ->
            makeImproper (d :| ds') <$> (dot >> datum)
              <|> return (List ds)
    )
    <|> abbreviation
    <?> "list"

abbreviation :: CharParser p => p Datum
abbreviation = do
  s <- Lexeme . Sym <$> abbrevPrefix
  d <- datum
  return $ List [s, d]

abbrevPrefix :: CharParser p => p String
abbrevPrefix =
  symbol "'" $> "quote"
    <|> symbol "`" $> "quasiquote"
    <|> symbol "," $> "unquote"
    <|> symbol ",@" $> "unquote-splicing"
    <|> symbol "#'" $> "syntax"
    <|> symbol "#`" $> "quasisyntax"
    <|> symbol "#," $> "unsyntax"
    <|> symbol "#,@" $> "unsyntax-splicing"

makeImproper :: NonEmpty Datum -> Datum -> Datum
makeImproper ds (Lexeme c) = ImproperList ds c
makeImproper ds (List ds') = List (NonEmpty.toList ds <> ds')
makeImproper ds (ImproperList ds' d) = makeImproper (ds <> ds') (Lexeme d)
