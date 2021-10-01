module My.Control.Monad.Trans.Identity
  ( IdentityT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..))
import My.Control.Monad.Trans.Class (MonadTrans (..))

-- | The identity monad transformer maps a monad to an equivalent monad.
newtype IdentityT m a = IdentityT {runIdentityT :: m a}

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT m) = IdentityT $ fmap f m

instance Applicative m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT f) <*> (IdentityT x) = IdentityT $ f <*> x

instance Alternative m => Alternative (IdentityT m) where
  empty = IdentityT empty
  (IdentityT x) <|> (IdentityT y) = IdentityT $ x <|> y

instance Monad m => Monad (IdentityT m) where
  return = IdentityT . return
  (IdentityT m) >>= f = IdentityT $ m >>= runIdentityT . f

instance MonadPlus m => MonadPlus (IdentityT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans IdentityT where
  lift = IdentityT
