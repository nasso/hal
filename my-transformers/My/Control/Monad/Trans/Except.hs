{-# LANGUAGE FunctionalDependencies #-}

module My.Control.Monad.Trans.Except
  ( MonadExcept (..),
    liftEither,
  )
where

-- | A monad that supports error handling.
class Monad m => MonadExcept e m | m -> e where
  -- | Throw an error.
  throwError :: e -> m a

  -- | Catch an error.
  catchError :: m a -> (e -> m a) -> m a

-- | Lift an @Either@ value into the @MonadExcept@ monad.
liftEither :: MonadExcept e m => Either e a -> m a
liftEither = either throwError return
