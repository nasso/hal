module Control.Monad.Trans.Maybe
  ( MaybeT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))

-- | The maybe monad transformer.
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f m = MaybeT $ fmap (fmap f) (runMaybeT m)

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT $ pure (pure a)
  mf <*> ma = MaybeT $ (<*>) <$> runMaybeT mf <*> runMaybeT ma

instance Alternative m => Alternative (MaybeT m) where
  empty = MaybeT empty
  m <|> n = MaybeT $ (<|>) <$> runMaybeT m <*> runMaybeT n

instance Monad m => Monad (MaybeT m) where
  return = pure
  m >>= f = MaybeT $ do
    v <- runMaybeT m
    case v of
      Nothing -> return Nothing
      Just x -> runMaybeT (f x)

instance MonadPlus m => MonadPlus (MaybeT m)

instance Monad m => MonadFail (MaybeT m) where
  fail _ = MaybeT $ return Nothing

instance MonadTrans MaybeT where
  lift m = MaybeT $ Just <$> m

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
