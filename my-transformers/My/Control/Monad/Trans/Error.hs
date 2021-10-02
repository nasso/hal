{-# LANGUAGE FunctionalDependencies #-}

module My.Control.Monad.Trans.Error
  ( MonadError (..),
    liftEither,
  )
where

-- | A monad that supports error handling.
class Monad m => MonadError e m | m -> e where
  -- | Throw an error.
  throwError :: e -> m a

  -- | Catch an error.
  catchError :: m a -> (e -> m a) -> m a

-- | Lift an @Either@ value into the @MonadError@ monad.
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return
