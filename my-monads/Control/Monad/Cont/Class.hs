module Control.Monad.Cont.Class
  ( MonadCont (..),
  )
where

-- | A monad representing a computation in continuation passing style.
class Monad m => MonadCont m where
  -- | Call a function with the current continuation.
  callCC :: ((a -> m b) -> m a) -> m a
