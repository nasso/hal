{-# LANGUAGE TypeFamilies #-}

module Data.Stream
  ( Stream (..),
  )
where

import Data.Data (Proxy)
import Data.Kind (Type)

class Stream s where
  type Item s :: Type
  type Chunk s :: Type

  next :: s -> Maybe (Item s, s)
  nextWhile :: (Item s -> Bool) -> s -> (Chunk s, s)
  nextN :: Int -> s -> (Chunk s, s)

  makeChunk :: Proxy s -> [Item s] -> Chunk s
  unmakeChunk :: Proxy s -> Chunk s -> [Item s]

instance Stream [a] where
  type Item [a] = a
  type Chunk [a] = [a]

  next [] = Nothing
  next (x : xs) = Just (x, xs)

  nextWhile _ [] = ([], [])
  nextWhile p (x : xs)
    | p x = let (ys, zs) = nextWhile p xs in (x : ys, zs)
    | otherwise = ([], x : xs)

  nextN = splitAt

  makeChunk _ = id
  unmakeChunk _ = id
