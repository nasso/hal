{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module My.Control.Monad.Trans
  ( MonadTrans (..),
  )
where

class MonadTrans t where
  -- | Lift a computation from the argument monad to the constructed monad.
  lift :: Monad m => m a -> t m a
