{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Parser.Class
  ( module Control.Monad.Parser.Class,
  )
where

import Control.Applicative ((<**>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Stream

infixl 3 <|>

infixl 1 <?>

-- | A monad that parses a stream of items.
class Monad m => MonadParser s m | m -> s where
  -- | Return the input stream.
  getInput :: m s

  -- | Replace the input stream.
  setInput :: s -> m ()

  -- | A parser that always fails.
  noParse :: m a

  -- | Parse the next item.
  item :: m (Item s)

  -- | A parser that only succeeds if the given parser fails.
  notFollowedBy :: m a -> m ()

  -- | Attempt to parse @p@, if it fails, try @q@.
  (<|>) :: m a -> m a -> m a

  -- | Label a parser with a name for error messages.
  (<?>) :: m a -> String -> m a

-- | Parser that succeeds if the stream is empty. Does not consume any items.
eof :: MonadParser s m => m ()
eof = notFollowedBy item <?> "end of input"

-- | Fail with an "expected" message.
expected :: MonadParser s m => String -> m a
expected s = noParse <?> s

-- | Succeeds only if the value parsed by the parser satisfies the predicate.
satisfy :: MonadParser s m => m a -> (a -> Bool) -> m a
satisfy p f = p >>= \i -> if f i then pure i else noParse

-- | Parse a single item satisfying the given predicate.
match :: MonadParser s m => (Item s -> Bool) -> m (Item s)
match = satisfy item

-- | Make a parser optional.
optional :: MonadParser s m => m a -> m (Maybe a)
optional p = Just <$> p <|> pure Nothing

-- | Try to run the given parser as many times as possible.
many :: MonadParser s m => m a -> m [a]
many p = ((:) <$> p <*> many p) <|> pure []

-- | Try to run the given parser as many times as possible, but at least once.
some :: MonadParser s m => m a -> m [a]
some p = (:) <$> p <*> many p

-- | Same as @some@, but the result is returned as a NonEmpty list.
some1 :: MonadParser s m => m a -> m (NonEmpty a)
some1 p = (:|) <$> p <*> many p

-- | Parse a series of `a` separated by `b`s (without a leading `b`).
sepBy :: MonadParser s m => m a -> m b -> m [a]
sepBy w s = do
  a <- w
  rest <- many (s >> w)
  return $ a : rest

-- | Parse any value equal to `a`.
-- This version accepts non-Show items.
like' :: (MonadParser s m, Eq (Item s)) => Item s -> m (Item s)
like' a = item `satisfy` (== a)

-- | Parse any value not equal to `a`.
-- This version accepts non-Show items.
unlike' :: (MonadParser s m, Eq (Item s)) => Item s -> m (Item s)
unlike' a = item `satisfy` (/= a)

-- | Parse any value equal to at least one element of the given list.
-- This version accepts non-Show items.
oneOf' :: (MonadParser s m, Eq (Item s)) => [Item s] -> m (Item s)
oneOf' l = item `satisfy` (`elem` l)

-- | Parse any value not equivalent to any element of the given list.
-- This version accepts non-Show items.
noneOf' :: (MonadParser s m, Eq (Item s)) => [Item s] -> m (Item s)
noneOf' l = item `satisfy` (`notElem` l)

-- | Parse a continuous sequence of items equal to the given one.
-- This version accepts non-Show items.
string' :: (MonadParser s m, Eq (Item s)) => [Item s] -> m [Item s]
string' [] = return []
string' (x : xs) = like' x >> string' xs >> return (x : xs)

-- | Parse any value equal to `a`.
-- For a version that accepts non-Show items, see @like'@.
like :: (MonadParser s m, Eq (Item s), Show (Item s)) => Item s -> m (Item s)
like a = like' a <?> show a

-- | Parse any value not equal to `a`.
-- For a version that accepts non-Show items, see @unlike'@.
unlike :: (MonadParser s m, Eq (Item s), Show (Item s)) => Item s -> m (Item s)
unlike a = unlike' a <?> "anything but " ++ show a

-- | Parse any value equal to at least one element of the given list.
-- For a version that accepts non-Show items, see @oneOf'@.
oneOf :: (MonadParser s m, Eq (Item s), Show (Item s)) => [Item s] -> m (Item s)
oneOf l = oneOf' l <?> "one of " ++ show l

-- | Parse any value not equivalent to any element of the given list.
-- For a version that accepts non-Show items, see @noneOf'@.
noneOf ::
  (MonadParser s m, Eq (Item s), Show (Item s)) =>
  [Item s] ->
  m (Item s)
noneOf l = noneOf' l <?> "none of " ++ show l

-- | Parse a continuous sequence of items equal to the given one.
-- For a version that accepts non-Show items, see @string'@.
string ::
  (MonadParser s m, Eq (Item s), Show (Item s)) =>
  [Item s] ->
  m [Item s]
string s = string' s <?> show s

-- | `chainl1 p op` Parse a chain of *one* or more occurrences of `p`,
-- separated by `op`. Return a value obtained by a left associative application
-- of all functions returned by `op` to the values returned by `p`.
--
-- This is particularly useful for parsing left associative infix operators.
chainl1 :: MonadParser s m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

-- | `chainr1 p op` Parse a chain of one or more occurrences of `p`,
-- separated by `op`. Return a value obtained by a right associative application
-- of all functions returned by `op` to the values returned by `p`.
--
-- This is particularly useful for parsing right associative infix operators.
chainr1 :: MonadParser s m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan
  where
    scan = p <**> rst
    rst = (flip <$> op <*> scan) <|> pure id

-- | Run a parser on a different stream of items.
exec :: MonadParser s m => s -> m a -> m (a, s)
exec s' p = do
  s <- getInput
  setInput s'
  x <- p
  s'' <- getInput
  setInput s
  return (x, s'')
