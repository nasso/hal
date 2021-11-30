{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ListStream (ListStream (..)) where

import Data.Stream

data ListStream a = ListStream [a] Int

instance Stream (ListStream a) where
  type Item (ListStream a) = a
  type Chunk (ListStream a) = [a]
  type Pos (ListStream a) = Int

  next (ListStream [] _) = Nothing
  next (ListStream (x : xs) p) = Just (x, ListStream xs (p + 1))

  nextWhile f s = case next s of
    Nothing -> ([], s)
    Just (x, s') | f x -> let (xs, s'') = nextWhile f s' in (x : xs, s'')
    _ -> ([], s)

  nextN 0 s = ([], s)
  nextN n s = case next s of
    Nothing -> ([], s)
    Just (x, s') -> (x : xs, s'') where (xs, s'') = nextN (n - 1) s'

  makeChunk _ = id
  unmakeChunk _ = id

  getPos (ListStream _ p) = p
