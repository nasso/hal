{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module My.Control.Monad.Trans.ReaderT
  ( module My.Control.Monad.Trans.Reader,
    ReaderT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import My.Control.Monad.Trans (MonadTrans (..))
import My.Control.Monad.Trans.Except
import My.Control.Monad.Trans.IO
import My.Control.Monad.Trans.Parser
import My.Control.Monad.Trans.Reader
import My.Control.Monad.Trans.State

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

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ReaderT r m) where
  get = lift get
  put s = lift $ put s

instance MonadExcept e m => MonadExcept e (ReaderT r m) where
  throwError e = ReaderT $ \_ -> throwError e
  catchError m f = ReaderT $ \r ->
    runReaderT m r `catchError` \e -> runReaderT (f e) r

instance MonadParser p m => MonadParser p (ReaderT r m) where
  item = lift item
  exec ts e = ReaderT $ exec ts . runReaderT e
