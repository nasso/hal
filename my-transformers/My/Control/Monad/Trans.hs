{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module My.Control.Monad.Trans
  ( module My.Control.Monad.Trans,
    module My.Control.Monad.Trans.Class,
    module My.Control.Monad.Trans.Error,
    module My.Control.Monad.Trans.IO,
    module My.Control.Monad.Trans.Parser,
    module My.Control.Monad.Trans.Reader,
    module My.Control.Monad.Trans.State,
    module My.Control.Monad.Trans.Maybe,
    module My.Control.Monad.Trans.Identity,
  )
where

import My.Control.Monad.Trans.Class
import My.Control.Monad.Trans.Error
import My.Control.Monad.Trans.IO
import My.Control.Monad.Trans.Identity
import My.Control.Monad.Trans.Maybe
import My.Control.Monad.Trans.Parser
import My.Control.Monad.Trans.Reader
import My.Control.Monad.Trans.State

instance MonadIO m => MonadIO (ErrorT e m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (IdentityT m) where
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

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO

instance MonadError e m => MonadError e (StateT s m) where
  throwError e = StateT $ \_ -> throwError e
  catchError st f =
    StateT $ \s -> catchError (runStateT st s) (flip runStateT s . f)

instance MonadReader r m => MonadReader r (StateT s m) where
  ask = lift ask
  local f st = StateT $ \s -> local f (runStateT st s)

instance MonadParser p m => MonadParser p (StateT s m) where
  next = lift next
  exec ts e = StateT $ \s -> do
    ((a, s'), p) <- exec ts (runStateT e s)
    return ((a, p), s')

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ReaderT r m) where
  get = lift get
  put s = lift $ put s

instance MonadError e m => MonadError e (ReaderT r m) where
  throwError e = ReaderT $ \_ -> throwError e
  catchError m f = ReaderT $ \r ->
    runReaderT m r `catchError` \e -> runReaderT (f e) r

instance MonadParser p m => MonadParser p (ReaderT r m) where
  next = lift next
  exec ts e = ReaderT $ exec ts . runReaderT e

instance MonadIO m => MonadIO (ParserT t m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ParserT t m) where
  get = lift get
  put s = lift $ put s

instance MonadError e m => MonadError e (ParserT t m) where
  throwError e = ParserT $ const $ throwError e
  catchError m f = ParserT $ \s -> do
    runParserT m s `catchError` \e -> runParserT (f e) s

instance MonadReader r m => MonadReader r (ParserT t m) where
  ask = lift ask
  local f st = ParserT $ \s -> local f (runParserT st s)
