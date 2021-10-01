{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module My.Control.Monad.Trans.Error
  ( MonadError (..),
    ErrorT (..),
    liftEither,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import My.Control.Monad.Trans.Class (MonadTrans (..))

-- | A monad that supports error handling.
class Monad m => MonadError e m | m -> e where
  -- | Throw an error.
  throwError :: e -> m a

  -- | Catch an error.
  catchError :: m a -> (e -> m a) -> m a

-- | Lift an @Either@ value into the @MonadError@ monad.
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

-- | A monad transformer that adds an error type to a monad.
newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}

instance Functor m => Functor (ErrorT e m) where
  fmap f (ErrorT m) = ErrorT $ fmap (fmap f) m

instance (Applicative m, Monad m) => Applicative (ErrorT e m) where
  pure a = ErrorT $ pure (Right a)
  mf <*> mx = ErrorT $ do
    mf' <- runErrorT mf
    mx' <- runErrorT mx
    case (mf', mx') of
      (Right f, Right x) -> return (Right (f x))
      (Left e, _) -> return (Left e)
      (_, Left e) -> return (Left e)

instance (Functor m, MonadPlus m) => Alternative (ErrorT e m) where
  empty = ErrorT empty
  eta <|> etb = ErrorT $ runErrorT eta <|> runErrorT etb

instance Monad m => Monad (ErrorT e m) where
  return a = ErrorT $ return (Right a)
  m >>= f = ErrorT $ runErrorT m >>= either (return . Left) (runErrorT . f)

instance MonadPlus m => MonadPlus (ErrorT e m)

instance Monad m => MonadError e (ErrorT e m) where
  throwError e = ErrorT $ return (Left e)
  catchError m f =
    ErrorT $ runErrorT m >>= either (runErrorT . f) (return . Right)

instance MonadTrans (ErrorT e) where
  lift m = ErrorT $ Right <$> m
