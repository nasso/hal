{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.State.Class
  ( MonadState (..),
    gets,
    modify,
    modify',
  )
where

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
