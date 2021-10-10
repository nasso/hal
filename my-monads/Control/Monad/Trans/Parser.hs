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

-- | Parser monad transformer.
newtype ParserT t m a = ParserT {runParserT :: [t] -> m (a, [t])}

instance Functor m => Functor (ParserT t m) where
  fmap f (ParserT p) = ParserT $ fmap (first f) . p

instance (Applicative m, Monad m) => Applicative (ParserT t m) where
  pure a = ParserT $ \s -> pure (a, s)
  mf <*> mx = ParserT $ \ts -> do
    (f, ts') <- runParserT mf ts
    (x, ts'') <- runParserT mx ts'
    pure (f x, ts'')

instance (Functor m, MonadPlus m) => Alternative (ParserT t m) where
  empty = ParserT $ const empty
  pta <|> ptb = ParserT $ \ts -> runParserT pta ts <|> runParserT ptb ts

instance Monad m => Monad (ParserT t m) where
  return = pure
  m >>= f = ParserT $ \ts -> do
    (a, ts') <- runParserT m ts
    runParserT (f a) ts'

instance MonadPlus m => MonadPlus (ParserT t m)

instance (Monad m, MonadPlus m) => MonadParser t (ParserT t m) where
  item = ParserT eat
    where
      eat [] = empty
      eat (x : xs) = return (x, xs)

  eof = ParserT eat
    where
      eat [] = pure ((), [])
      eat _ = empty

  exec ts p = ParserT $ \s -> do
    r <- runParserT p ts
    pure (r, s)

instance MonadTrans (ParserT t) where
  lift m = ParserT $ \s -> do
    a <- m
    return (a, s)

instance MonadFail m => MonadFail (ParserT t m) where
  fail = ParserT . const . fail

instance MonadIO m => MonadIO (ParserT t m) where
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (ParserT t m) where
  get = lift get
  put s = lift $ put s

instance MonadExcept e m => MonadExcept e (ParserT t m) where
  throwError e = ParserT $ const $ throwError e
  catchError m f = ParserT $ \s -> do
    runParserT m s `catchError` \e -> runParserT (f e) s

instance MonadReader r m => MonadReader r (ParserT t m) where
  ask = lift ask
  local f st = ParserT $ \s -> local f (runParserT st s)

instance MonadCont m => MonadCont (ParserT t m) where
  callCC f =
    ParserT $
      \s -> callCC $
        \k -> runParserT (f $ \a -> ParserT $ \s' -> k (a, s')) s
