{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Parser.Class
  ( MonadParser (..),
    match,
    sepBy,
    like,
    unlike,
    oneOf,
    noneOf,
    string,
    chainl1,
    chainr1,
  )
where

import Control.Applicative (Alternative (empty, many, (<|>)), (<**>))
import Control.Monad (MonadPlus)
import Data.Stream

-- | A monad that parses a stream of items.
class (Monad m, MonadPlus m, Stream s) => MonadParser s m | m -> s where
  -- | Parse the next item.
  item :: m (Item s)

  -- | Parser that succeeds if the stream is empty. Does not consume any items.
  eof :: m ()

  -- | Run a parser on a different stream of items.
  exec :: s -> m a -> m (a, s)

-- | Parse a single item satisfying the given predicate.
match :: MonadParser s m => (Item s -> Bool) -> m (Item s)
match f = do
  x <- item
  if f x
    then return x
    else empty

-- | Parse a series of `a` separated by `b`s (without a leading `b`).
sepBy :: MonadParser s m => m a -> m b -> m [a]
sepBy w s = do
  a <- w
  rest <- many (s >> w)
  return $ a : rest

-- | Parse any value equal to `a`.
like :: (MonadParser s m, Eq (Item s)) => Item s -> m (Item s)
like a = match (== a)

-- | Parse any value not equal to `a`.
unlike :: (MonadParser s m, Eq (Item s)) => Item s -> m (Item s)
unlike a = match (/= a)

-- | Parse any value equal to at least one element of the given list.
oneOf :: (MonadParser s m, Eq (Item s)) => [Item s] -> m (Item s)
oneOf l = match (`elem` l)

-- | Parse any value not equivalent to any element of the given list.
noneOf :: (MonadParser s m, Eq (Item s)) => [Item s] -> m (Item s)
noneOf l = match (`notElem` l)

-- | Parse a continuous sequence of items equal to the given one.
string :: (MonadParser s m, Eq (Item s)) => [Item s] -> m [Item s]
string [] = return []
string (x : xs) = like x >> string xs >> return (x : xs)

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
