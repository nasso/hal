module Parsing
  ( module Parsing,
  )
where

import Control.Applicative (Alternative (empty, many, (<|>)), (<**>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)

-- | A parser from `String` to values of type `a`.
newtype Parser a = Parser
  { parse :: String -> Maybe (a, String)
  }

-- impl Functor for Parser
instance Functor Parser where
  fmap ab ap =
    Parser
      ( \s -> case parse ap s of
          Nothing -> Nothing
          (Just (a, rest)) -> Just (ab a, rest)
      )

-- impl Applicative for Parser
instance Applicative Parser where
  pure v = Parser (\s -> Just (v, s))
  abp <*> ap =
    Parser
      ( \s -> case parse abp s of
          Nothing -> Nothing
          (Just (ab, rest)) -> parse (ab <$> ap) rest
      )

-- impl Monad for Parser
-- gives us >>= and >> to chain parsers
-- we also get to use return and do
instance Monad Parser where
  ap >>= bp =
    Parser
      ( \s -> case parse ap s of
          Nothing -> Nothing
          (Just (a, rest)) -> parse (bp a) rest
      )

-- impl Alternative for Parser
-- this is used mainly for <|> to choose between two parsers
instance Alternative Parser where
  empty = Parser (const Nothing)
  a <|> b =
    Parser
      ( \s -> case parse a s of
          Nothing -> parse b s
          res -> res
      )

-- primitive parsers

-- | Parse a single character.
getc :: Parser Char
getc = Parser r
  where
    r [] = Nothing
    r (c : cs) = Just (c, cs)

-- | Parse a single character satisfying the given predicate.
match :: (Char -> Bool) -> Parser Char
match f = do
  x <- getc
  if f x
    then return x
    else empty

-- | Parse a series of `a` separated by `b`s (without a leading `b`).
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy w s = do
  first <- w
  rest <- many (s >> w)
  return $ first : rest

-- | Parse a unicode whitespace character (space, tab, newline, etc.).
space :: Parser Char
space = match isSpace

-- | Parse a decimal digit.
digit :: Parser Char
digit = match isDigit

-- | Parse a lowercase letter.
lower :: Parser Char
lower = match isLower

-- | Parse an uppercase letter.
upper :: Parser Char
upper = match isUpper

-- | Parse any letter.
alpha :: Parser Char
alpha = match isAlpha

-- | Parse any letter or digit.
alphanum :: Parser Char
alphanum = match isAlphaNum

-- | Parse a specific character.
char :: Char -> Parser Char
char c = match (== c)

-- | Parse any character contained in the given string.
oneOf :: String -> Parser Char
oneOf s = foldl (<|>) empty (map char s)

-- | Parse any character not contained in the given string.
noneOf :: String -> Parser Char
noneOf s = match (`notElem` s)

-- | Parse exactly the given string.
literal :: String -> Parser String
literal [] = return []
literal (x : xs) = char x >> literal xs >> return (x : xs)

-- | Make a parser consume any trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* many space

-- | Parse exactly the given string, plus zero or more trailing whitespace.
symbol :: String -> Parser String
symbol = lexeme . literal

-- | `chainl1 p op` Parse a chain of *one* or more occurrences of `p`,
-- separated by `op`. Return a value obtained by a left associative application
-- of all functions returned by `op` to the values returned by `p`.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

-- | `chainr1 p op` Parse a chain of one or more occurrences of `p`,
-- separated by `op`. Return a value obtained by a right associative application
-- of all functions returned by `op` to the values returned by `p`.
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan = p <**> rst
    rst = (flip <$> op <*> scan) <|> pure id

-- | Parse a value wrapped in parentheses or square brackets.
paren :: Parser a -> Parser a
paren p = symbol "(" *> p <* symbol ")" <|> symbol "[" *> p <* symbol "]"
