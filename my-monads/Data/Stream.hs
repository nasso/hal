{-# LANGUAGE TypeFamilies #-}

module Data.Stream
  ( Stream (..),
    ListStream (..),
    LinePos (..),
    LineStream (..),
    makeLineStream,
  )
where

import Data.Data (Proxy (..))
import Data.Kind (Type)

class Stream s where
  type Item s :: Type
  type Chunk s :: Type
  type Pos s :: Type

  next :: s -> Maybe (Item s, s)
  nextWhile :: (Item s -> Bool) -> s -> (Chunk s, s)
  nextN :: Int -> s -> (Chunk s, s)

  makeChunk :: Proxy s -> [Item s] -> Chunk s
  unmakeChunk :: Proxy s -> Chunk s -> [Item s]

  getPos :: s -> Pos s

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

data LinePos = LinePos Int Int String

instance Show LinePos where
  show (LinePos l c ls) =
    lnum ++ ":" ++ cnum ++ ":\n" ++ snippet
    where
      lnum = show (l + 1)
      cnum = show (c + 1)
      gut = replicate (length lnum) ' ' ++ " | "
      ngut = lnum ++ " | "
      snippet = gut ++ "\n" ++ ngut ++ ls ++ "\n" ++ gut ++ cursor
      cursor = replicate c ' ' ++ "^"

instance Eq LinePos where
  (LinePos l1 c1 _) == (LinePos l2 c2 _) = l1 == l2 && c1 == c2

instance Ord LinePos where
  compare (LinePos l1 c1 _) (LinePos l2 c2 _) =
    case compare l1 l2 of
      EQ -> compare c1 c2
      x -> x

start :: String -> LinePos
start s = LinePos 0 0 $ takeWhile (/= '\n') s

adv :: LinePos -> LinePos
adv (LinePos l c s) = LinePos l (c + 1) s

nextl :: LinePos -> String -> LinePos
nextl (LinePos l _ _) s = LinePos (l + 1) 0 (takeWhile (/= '\n') s)

data LineStream = LineStream String LinePos

instance Stream LineStream where
  type Item LineStream = Char
  type Chunk LineStream = String
  type Pos LineStream = LinePos

  next (LineStream [] _) = Nothing
  next (LineStream ('\n' : xs) p) = Just ('\n', LineStream xs (nextl p xs))
  next (LineStream (x : xs) p) = Just (x, LineStream xs (adv p))

  nextWhile f s = case next s of
    Nothing -> ([], s)
    Just (x, s') | f x -> (x : xs, s'') where (xs, s'') = nextWhile f s'
    _ -> ([], s)

  nextN 0 s = ([], s)
  nextN n s = case next s of
    Nothing -> ([], s)
    Just (x, s') -> (x : xs, s'') where (xs, s'') = nextN (n - 1) s'

  makeChunk _ = id
  unmakeChunk _ = id

  getPos (LineStream _ p) = p

makeLineStream :: String -> LineStream
makeLineStream s = LineStream s (start s)
