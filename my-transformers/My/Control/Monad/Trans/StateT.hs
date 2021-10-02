{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module My.Control.Monad.Trans.StateT
  ( module My.Control.Monad.Trans.State,
    StateT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Data.Bifunctor (Bifunctor (first))
import My.Control.Monad.Trans (MonadTrans (..))
import My.Control.Monad.Trans.Error
import My.Control.Monad.Trans.IO
import My.Control.Monad.Trans.Parser
import My.Control.Monad.Trans.Reader
import My.Control.Monad.Trans.State

-- | StateT is a monad transformer that adds state to a monad.
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap f st = StateT $ fmap (first f) . runStateT st

instance (Applicative m, Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  stf <*> stx = StateT $ \s -> do
    (f, s') <- runStateT stf s
    (x, s'') <- runStateT stx s'
    pure (f x, s'')

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
  empty = StateT $ const empty
  sta <|> stb = StateT $ \s -> runStateT sta s <|> runStateT stb s

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  m >>= f = StateT $ \s -> do
    (a, s') <- runStateT m s
    runStateT (f a) s'

instance MonadPlus m => MonadPlus (StateT s m)

instance Monad m => MonadState s (StateT s m) where
  get = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return ((), s)

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do a <- m; return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadError e m => MonadError e (StateT s m) where
  throwError e = StateT $ \_ -> throwError e
  catchError st f =
    StateT $ \s -> catchError (runStateT st s) (flip runStateT s . f)

instance MonadReader r m => MonadReader r (StateT s m) where
  ask = lift ask
  local f st = StateT $ \s -> local f (runStateT st s)

instance MonadParser p m => MonadParser p (StateT s m) where
  next = lift next
  exec ts e = StateT $ \s -> do
    ((a, s'), p) <- exec ts (runStateT e s)
    return ((a, p), s')
