{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module My.Control.Monad.Trans.ErrorT
  ( module My.Control.Monad.Trans.Error,
    ErrorT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import My.Control.Monad.Trans (MonadTrans (..))
import My.Control.Monad.Trans.Error
import My.Control.Monad.Trans.IO
import My.Control.Monad.Trans.Parser
import My.Control.Monad.Trans.Reader
import My.Control.Monad.Trans.State

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

instance MonadIO m => MonadIO (ErrorT e m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ErrorT e m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (ErrorT e m) where
  ask = lift ask
  local f st = ErrorT $ local f (runErrorT st)

instance MonadParser t m => MonadParser t (ErrorT e m) where
  next = lift next
  exec ts e = ErrorT $ do
    v <- exec ts $ runErrorT e
    case v of
      (Left e', _) -> return $ Left e'
      (Right a, ts') -> return $ Right (a, ts')
