{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Cont
  ( module Control.Monad.Cont.Class,
    ContT (..),
    evalContT,
  )
where

import Control.Monad.Cont.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class (MonadTrans (..))

-- | A monad transformer that adds continuation passing to another monad.
newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}

instance Functor (ContT r m) where
  fmap f k = ContT $ \k' -> runContT k (k' . f)

instance Applicative (ContT r m) where
  pure a = ContT $ \k -> k a
  mf <*> mx = ContT $ \k -> runContT mf $ \f -> runContT mx $ \x -> k (f x)

instance Monad (ContT r m) where
  return = pure
  m >>= f = ContT $ \k -> runContT m $ \a -> runContT (f a) k

instance MonadCont (ContT r m) where
  callCC f = ContT $ \k -> runContT (f (\a -> ContT $ \_ -> k a)) k

instance MonadTrans (ContT r) where
  lift m = ContT $ \k -> m >>= k

instance MonadIO m => MonadIO (ContT r m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ContT r m) where
  get = lift get
  put = lift . put

instance MonadReader r' m => MonadReader r' (ContT r m) where
  ask = lift ask
  local f st = ContT $ \k -> local f (runContT st k)

-- | Equivalent to calling @runContT@ with @pure@ as the final continuation.
evalContT :: Monad m => ContT r m r -> m r
evalContT k = runContT k pure