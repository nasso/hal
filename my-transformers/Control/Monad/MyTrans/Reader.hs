{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.MyTrans.Reader
  ( MonadReader (..),
    asks,
  )
where

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
