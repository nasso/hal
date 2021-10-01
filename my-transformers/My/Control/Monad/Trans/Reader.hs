{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module My.Control.Monad.Trans.Reader
  ( MonadReader (..),
    ReaderT (..),
    asks,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import My.Control.Monad.Trans.Class (MonadTrans (..))

-- | A monad that carries an immutable environment.
class Monad m => MonadReader r m | m -> r where
  -- | Retrieve the environment.
  ask :: m r
  ask = reader id

  -- | Execute a computation in a modified environment.
  local :: (r -> r) -> m a -> m a

  -- | Retrieve a function of the current environment.
  reader :: (r -> a) -> m a
  reader f = f <$> ask

-- | Retrieve a function of the current environment.
asks :: MonadReader r m => (r -> a) -> m a
asks = reader

-- | Reader monad transformer.
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance (Applicative m, Monad m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ const $ pure a
  mf <*> mx = ReaderT $ \r -> do
    f <- runReaderT mf r
    x <- runReaderT mx r
    pure (f x)

instance (Functor m, MonadPlus m) => Alternative (ReaderT r m) where
  empty = ReaderT $ const empty
  eta <|> etb = ReaderT $ \r -> runReaderT eta r <|> runReaderT etb r

instance Monad m => Monad (ReaderT r m) where
  return a = ReaderT $ \_ -> return a
  m >>= f = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (f a) r

instance MonadPlus m => MonadPlus (ReaderT r m)

instance Monad m => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local f m = ReaderT $ runReaderT m . f

instance MonadTrans (ReaderT r) where
  lift m = ReaderT $ const m
