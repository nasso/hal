{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Parser
  ( module Control.Monad.Parser.Class,
    ParserT (..),
    ParseResult (..),
    ParseError (..),
    ErrorDesc (..),
  )
where

import Control.Monad.Cont.Class
import Control.Monad.Except.Class
import Control.Monad.IO.Class
import Control.Monad.Parser.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (intercalate)
import Data.Stream (Stream (..))

data ErrorDesc = Expected String | Note String

data ParseError p = ParseError p [ErrorDesc]

instance Show p => Show (ParseError p) where
  show (ParseError p d) =
    show p ++ "\n" ++ showExpects expects ++ showNotes notes
    where
      expects = [e | Expected e <- d]
      notes = [n | Note n <- d]
      showExpects [] = ""
      showExpects es = "expected " ++ intercalate ", " es ++ "\n"
      showNotes [] = ""
      showNotes (n : ns) = "note: " ++ n ++ "\n" ++ showNotes ns

joinErrors :: Ord p => ParseError p -> ParseError p -> ParseError p
joinErrors e1@(ParseError p1 d1) e2@(ParseError p2 d2)
  | p1 > p2 = e1
  | p1 < p2 = e2
  | otherwise = ParseError p1 (d1 ++ d2)

data ParseResult v s
  = Parsed v s
  | NoParse (ParseError (Pos s))

instance (Show v, Show (Pos s)) => Show (ParseResult v s) where
  show (Parsed v _) = show v
  show (NoParse e) = show e

-- | Parser monad transformer.
newtype ParserT s m a = ParserT {runParserT :: s -> m (ParseResult a s)}

instance Functor m => Functor (ParserT s m) where
  fmap f p = ParserT (fmap t . runParserT p)
    where
      t (NoParse e) = NoParse e
      t (Parsed a s) = Parsed (f a) s

instance (Applicative m, Monad m) => Applicative (ParserT s m) where
  pure a = ParserT $ \s -> pure (Parsed a s)
  mf <*> mx = ParserT $ \s -> do
    f <- runParserT mf s
    case f of
      Parsed fab s' -> do
        x <- runParserT mx s'
        case x of
          Parsed a s'' -> pure (Parsed (fab a) s'')
          NoParse pes -> pure (NoParse pes)
      NoParse pes -> pure (NoParse pes)

instance Monad m => Monad (ParserT s m) where
  return = pure
  m >>= f = ParserT $ \s -> do
    r <- runParserT m s
    case r of
      Parsed v s' -> runParserT (f v) s'
      NoParse e -> pure $ NoParse e

instance (Applicative m, Monad m, Stream s) => MonadFail (ParserT s m) where
  fail msg = ParserT $ \s -> pure $ NoParse $ ParseError (getPos s) [Note msg]

instance (Monad m, Stream s, Ord (Pos s)) => MonadParser s (ParserT s m) where
  getInput = ParserT $ \s -> pure $ Parsed s s

  setInput s = ParserT $ \_ -> pure $ Parsed () s

  noParse = ParserT $ \s -> pure $ NoParse $ ParseError (getPos s) []

  item = ParserT $ pure . eat
    where
      eat s = case next s of
        Nothing -> NoParse $ ParseError (getPos s) []
        Just (x, s') -> Parsed x s'

  notFollowedBy p = ParserT $ \s -> do
    r <- runParserT p s
    case r of
      Parsed _ _ -> pure $ NoParse $ ParseError (getPos s) []
      NoParse _ -> pure $ Parsed () s

  p <|> q = ParserT $ \s -> runParserT p s >>= first s
    where
      first _ (Parsed a s') = pure $ Parsed a s'
      first s (NoParse e) = runParserT q s >>= second e
      second _ (Parsed a s') = pure $ Parsed a s'
      second e1 (NoParse e2) = pure $ NoParse $ joinErrors e1 e2

  p <?> n = ParserT $ \s -> do
    r <- runParserT p s
    case r of
      Parsed a s' -> pure $ Parsed a s'
      NoParse _ -> pure $ NoParse $ ParseError (getPos s) [Expected n]

instance MonadTrans (ParserT s) where
  lift m = ParserT $ \s -> do
    a <- m
    pure $ Parsed a s

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
        \k -> runParserT (f $ \a -> ParserT $ k . Parsed a) s
