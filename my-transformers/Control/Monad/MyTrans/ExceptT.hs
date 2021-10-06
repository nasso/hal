{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MyTrans.ExceptT
  ( module Control.Monad.MyTrans.Except,
    ExceptT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Control.Monad.MyTrans.Class (MonadTrans (..))
import Control.Monad.MyTrans.Except
import Control.Monad.MyTrans.IO
import Control.Monad.MyTrans.Parser
import Control.Monad.MyTrans.Reader
import Control.Monad.MyTrans.State

-- | A monad transformer that adds an error type to a monad.
newtype ExceptT e m a = ExceptT {runErrorT :: m (Either e a)}

instance Functor m => Functor (ExceptT e m) where
  fmap f (ExceptT m) = ExceptT $ fmap (fmap f) m

instance (Applicative m, Monad m) => Applicative (ExceptT e m) where
  pure a = ExceptT $ pure (Right a)
  mf <*> mx = ExceptT $ do
    mf' <- runErrorT mf
    mx' <- runErrorT mx
    case (mf', mx') of
      (Right f, Right x) -> return (Right (f x))
      (Left e, _) -> return (Left e)
      (_, Left e) -> return (Left e)

instance (Functor m, MonadPlus m) => Alternative (ExceptT e m) where
  empty = ExceptT empty
  eta <|> etb = ExceptT $ runErrorT eta <|> runErrorT etb

instance Monad m => Monad (ExceptT e m) where
  return a = ExceptT $ return (Right a)
  m >>= f = ExceptT $ runErrorT m >>= either (return . Left) (runErrorT . f)

instance MonadPlus m => MonadPlus (ExceptT e m)

instance Monad m => MonadExcept e (ExceptT e m) where
  throwError e = ExceptT $ return (Left e)
  catchError m f =
    ExceptT $ runErrorT m >>= either (runErrorT . f) (return . Right)

instance MonadTrans (ExceptT e) where
  lift m = ExceptT $ Right <$> m

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask = lift ask
  local f st = ExceptT $ local f (runErrorT st)

instance MonadParser t m => MonadParser t (ExceptT e m) where
  item = lift item
  exec ts e = ExceptT $ do
    v <- exec ts $ runErrorT e
    case v of
      (Left e', _) -> return $ Left e'
      (Right a, ts') -> return $ Right (a, ts')
