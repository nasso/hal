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

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Except.Class
import Control.Monad.IO.Class
import Control.Monad.Parser.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (union)
import Data.Stream (Stream (..))

data ErrorDesc = Expected String | Note String deriving (Eq)

data ParseError p = ParseError p [ErrorDesc]

makeOrList :: [String] -> String
makeOrList [] = ""
makeOrList [x] = x
makeOrList [a, b] = a ++ ", or " ++ b
makeOrList (x : xs) = x ++ ", " ++ makeOrList xs

instance Show p => Show (ParseError p) where
  show (ParseError p d) =
    show p ++ "\n" ++ showExpects expects ++ showNotes notes
    where
      expects = [e | Expected e <- d]
      notes = [n | Note n <- d]
      showExpects [] = ""
      showExpects es = "expected " ++ makeOrList es ++ "\n"
      showNotes [] = ""
      showNotes (n : ns) = "note: " ++ n ++ "\n" ++ showNotes ns

joinErrors :: Ord p => ParseError p -> ParseError p -> ParseError p
joinErrors e1@(ParseError p1 d1) e2@(ParseError p2 d2)
  | null d1 && not (null d2) = e2
  | null d2 && not (null d1) = e1
  | p1 > p2 = e1
  | p1 < p2 = e2
  | otherwise = ParseError p1 (d1 `union` d2)

emptyError :: Stream s => s -> ParseError (Pos s)
emptyError s = ParseError (getPos s) []

data ParseResult v s
  = Parsed v s (ParseError (Pos s))
  | NoParse (ParseError (Pos s))

instance (Show v, Show (Pos s)) => Show (ParseResult v s) where
  show (Parsed v _ _) = show v
  show (NoParse e) = show e

-- | Parser monad transformer.
newtype ParserT s m a = ParserT {runParserT :: s -> m (ParseResult a s)}

instance Functor m => Functor (ParserT s m) where
  fmap f p = ParserT (fmap t . runParserT p)
    where
      t (NoParse e) = NoParse e
      t (Parsed a s e) = Parsed (f a) s e

instance (Stream s, Applicative m, Monad m) => Applicative (ParserT s m) where
  pure a = ParserT $ \s -> pure (Parsed a s $ emptyError s)
  mf <*> mx = ParserT $ runParserT mf >=> pmf
    where
      pmf (NoParse e) = pure $ NoParse e
      pmf (Parsed f s e1) = pmx f e1 <$> runParserT mx s
      pmx _ e1 (NoParse e2) = NoParse $ joinErrors e1 e2
      pmx f e1 (Parsed x s' e2) = Parsed (f x) s' $ joinErrors e1 e2

instance (Stream s, Monad m) => Monad (ParserT s m) where
  return = pure
  m >>= f = ParserT $ runParserT m >=> first
    where
      first (NoParse e) = pure $ NoParse e
      first (Parsed r s e) = second e <$> runParserT (f r) s
      second e1 (NoParse e2) = NoParse $ joinErrors e1 e2
      second e1 (Parsed r s e2) = Parsed r s $ joinErrors e1 e2

instance (Applicative m, Monad m, Stream s) => MonadFail (ParserT s m) where
  fail msg = ParserT $ \s -> pure $ NoParse $ ParseError (getPos s) [Note msg]

instance (Monad m, Stream s) => MonadParser s (ParserT s m) where
  getInput = ParserT $ \s -> pure $ Parsed s s $ emptyError s

  setInput s = ParserT $ \_ -> pure $ Parsed () s $ emptyError s

  noParse = ParserT $ \s -> pure $ NoParse $ emptyError s

  item = ParserT $ pure . eat
    where
      eat s = case next s of
        Nothing -> NoParse $ emptyError s
        Just (x, s') -> Parsed x s' $ emptyError s'

  notFollowedBy p = ParserT $ \s -> go s <$> runParserT p s
    where
      go s (NoParse _) = Parsed () s $ emptyError s
      go s _ = NoParse $ emptyError s

  followedBy p = ParserT $ \s -> do
    r <- runParserT p s
    pure $ case r of
      (NoParse e) -> NoParse e
      _ -> Parsed () s $ emptyError s

  try p = ParserT $ \s -> do
    r <- runParserT p s
    pure $ case r of
      NoParse _ -> NoParse $ emptyError s
      _ -> r

  p <|> q = ParserT $ \s -> runParserT p s >>= first s
    where
      first _ (Parsed a s' e) = pure $ Parsed a s' e
      first s (NoParse e) = runParserT q s >>= second e
      second e1 (Parsed a s' e2) = pure $ Parsed a s' $ joinErrors e1 e2
      second e1 (NoParse e2) = pure $ NoParse $ joinErrors e1 e2

  p <?> n = ParserT $ \s -> labelize (getPos s) <$> runParserT p s
    where
      labelize here (Parsed a s e) = Parsed a s $ name here e
      labelize here (NoParse e) = NoParse $ name here e
      name here e@(ParseError pos _)
        | pos > here = e
        | otherwise = ParseError here [Expected n]

instance Stream s => MonadTrans (ParserT s) where
  lift m = ParserT $ \s -> do
    a <- m
    pure $ Parsed a s $ emptyError s

instance (Stream s, MonadIO m) => MonadIO (ParserT s m) where
  liftIO = lift . liftIO

instance (Stream s, MonadState s' m) => MonadState s' (ParserT s m) where
  get = lift get
  put s = lift $ put s

instance (Stream s, MonadExcept e m) => MonadExcept e (ParserT s m) where
  throwError e = ParserT $ const $ throwError e
  catchError m f = ParserT $ \s -> do
    runParserT m s `catchError` \e -> runParserT (f e) s

instance (Stream s, MonadReader r m) => MonadReader r (ParserT s m) where
  ask = lift ask
  local f st = ParserT $ \s -> local f (runParserT st s)

instance (Stream s, MonadCont m) => MonadCont (ParserT s m) where
  callCC f =
    ParserT $
      \s -> callCC $
        \k ->
          runParserT
            ( f $
                \a -> ParserT $ \s' -> k $ Parsed a s' $ emptyError s'
            )
            s
