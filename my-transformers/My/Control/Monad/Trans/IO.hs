module My.Control.Monad.Trans.IO (MonadIO (..)) where

class Monad m => MonadIO m where
  -- | Lift an IO computation into @m@.
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id
