{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module My.Control.Monad.Trans.State
  ( MonadState (..),
    StateT (..),
    gets,
    modify,
    modify',
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Data.Bifunctor (Bifunctor (first))
import My.Control.Monad.Trans.Class (MonadTrans (..))

-- | A monad that supports a state monad.
class Monad m => MonadState s m | m -> s where
  get :: m s
  get = state $ \s -> (s, s)

  put :: s -> m ()

  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let (a, s') = f s
     in put s' >> return a

-- | Get the current state.
gets :: MonadState s m => (s -> a) -> m a
gets f = f <$> get

-- | Modify the state.
modify :: MonadState s m => (s -> s) -> m ()
modify f = get >>= put . f

-- | Strict version of @modify@.
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do s <- get; put $! f s

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
