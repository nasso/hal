{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module My.Control.Monad.Trans.Parser
  ( MonadParser (..),
    eof,
    match,
    sepBy,
    like,
    oneOf,
    noneOf,
    literal,
    chainl1,
    chainr1,
    char,
    space,
    digit,
    lower,
    upper,
    alpha,
    alphanum,
    lexeme,
    symbol,
  )
where

import Control.Applicative (Alternative (empty, many, (<|>)), (<**>))
import Control.Monad (MonadPlus)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)

-- | A monad that parses a stream of tokens.
class (Monad m, MonadPlus m) => MonadParser t m | m -> t where
  -- | Read the next token.
  next :: m t

  -- | Run a parser on a different stream of tokens.
  exec :: [t] -> m a -> m (a, [t])

-- | Parse the end of the stream.
eof :: MonadParser t m => m ()
eof = (next >> empty) <|> return ()

-- | Parse a single token satisfying the given predicate.
match :: MonadParser t m => (t -> Bool) -> m t
match f = do
  x <- next
  if f x
    then return x
    else empty

-- | Parse a series of `a` separated by `b`s (without a leading `b`).
sepBy :: MonadParser t m => m a -> m b -> m [a]
sepBy w s = do
  a <- w
  rest <- many (s >> w)
  return $ a : rest

-- | Parse any value equal to `a`.
like :: (Eq t, MonadParser t m) => t -> m t
like a = match (== a)

-- | Parse any value equal to at least one element of the given list.
oneOf :: (Eq t, MonadParser t m) => [t] -> m t
oneOf l = match (`elem` l)

-- | Parse any value not equivalent to any element of the given list.
noneOf :: (Eq t, MonadParser t m) => [t] -> m t
noneOf l = match (`notElem` l)

-- | Parse a continuous sequence of tokens equal to the given one.
literal :: (Eq t, MonadParser t m) => [t] -> m [t]
literal [] = return []
literal (x : xs) = like x >> literal xs >> return (x : xs)

-- | `chainl1 p op` Parse a chain of *one* or more occurrences of `p`,
-- separated by `op`. Return a value obtained by a left associative application
-- of all functions returned by `op` to the values returned by `p`.
--
-- This is particularly useful for parsing left associative infix operators.
chainl1 :: MonadParser t m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

-- | `chainr1 p op` Parse a chain of one or more occurrences of `p`,
-- separated by `op`. Return a value obtained by a right associative application
-- of all functions returned by `op` to the values returned by `p`.
--
-- This is particularly useful for parsing right associative infix operators.
chainr1 :: MonadParser t m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan
  where
    scan = p <**> rst
    rst = (flip <$> op <*> scan) <|> pure id

-- | Parse a char equal to @c@.
char :: MonadParser Char m => Char -> m Char
char = like

-- | Parse a unicode whitespace character (space, tab, newline, etc.).
space :: MonadParser Char m => m Char
space = match isSpace

-- | Parse a decimal digit.
digit :: MonadParser Char m => m Char
digit = match isDigit

-- | Parse a lowercase letter.
lower :: MonadParser Char m => m Char
lower = match isLower

-- | Parse an uppercase letter.
upper :: MonadParser Char m => m Char
upper = match isUpper

-- | Parse any letter.
alpha :: MonadParser Char m => m Char
alpha = match isAlpha

-- | Parse any letter or digit.
alphanum :: MonadParser Char m => m Char
alphanum = match isAlphaNum

-- | Make a parser consume any trailing whitespace.
lexeme :: MonadParser Char m => m a -> m a
lexeme p = p <* many space

-- | Parse exactly the given string and discard any trailing whitespace.
symbol :: MonadParser Char m => String -> m String
symbol = lexeme . literal
