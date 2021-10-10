{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Parser
  ( module Control.Monad.Parser.Class,
    ParserT (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus)
import Control.Monad.Cont.Class
import Control.Monad.Except.Class
import Control.Monad.IO.Class
import Control.Monad.Parser.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Bifunctor (Bifunctor (first))
import Data.Stream (Stream (..))

-- | Parser monad transformer.
newtype ParserT s m a = ParserT {runParserT :: s -> m (a, s)}

instance Functor m => Functor (ParserT s m) where
  fmap f (ParserT p) = ParserT $ fmap (first f) . p

instance (Applicative m, Monad m) => Applicative (ParserT s m) where
  pure a = ParserT $ \s -> pure (a, s)
  mf <*> mx = ParserT $ \ts -> do
    (f, ts') <- runParserT mf ts
    (x, ts'') <- runParserT mx ts'
    pure (f x, ts'')

instance (Functor m, MonadPlus m) => Alternative (ParserT s m) where
  empty = ParserT $ const empty
  pta <|> ptb = ParserT $ \ts -> runParserT pta ts <|> runParserT ptb ts

instance Monad m => Monad (ParserT s m) where
  return = pure
  m >>= f = ParserT $ \ts -> do
    (a, ts') <- runParserT m ts
    runParserT (f a) ts'

instance MonadPlus m => MonadPlus (ParserT s m)

instance (Monad m, MonadPlus m, Stream s) => MonadParser s (ParserT s m) where
  item = ParserT eat
    where
      eat s = case next s of
        Nothing -> empty
        Just (x, s') -> pure (x, s')

  eof = ParserT eat
    where
      eat s = case next s of
        Nothing -> pure ((), s)
        Just _ -> empty

  exec ts p = ParserT $ \s -> do
    r <- runParserT p ts
    pure (r, s)

instance MonadTrans (ParserT s) where
  lift m = ParserT $ \s -> do
    a <- m
    return (a, s)

instance MonadFail m => MonadFail (ParserT s m) where
  fail = ParserT . const . fail

instance MonadIO m => MonadIO (ParserT s m) where
  liftIO = lift . liftIO

instance MonadState s' m => MonadState s' (ParserT s m) where
  get = lift get
  put s = lift $ put s

instance MonadExcept e m => MonadExcept e (ParserT s m) where
  throwError e = ParserT $ const $ throwError e
  catchError m f = ParserT $ \s -> do
    runParserT m s `catchError` \e -> runParserT (f e) s

instance MonadReader r m => MonadReader r (ParserT s m) where
  ask = lift ask
  local f st = ParserT $ \s -> local f (runParserT st s)

instance MonadCont m => MonadCont (ParserT s m) where
  callCC f =
    ParserT $
      \s -> callCC $
        \k -> runParserT (f $ \a -> ParserT $ \s' -> k (a, s')) s
