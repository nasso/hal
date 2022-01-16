{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ListStream (ListStream (..)) where

import Data.Stream

data ListStream a = ListStream [a] Int

instance Stream (ListStream a) where
  type Item (ListStream a) = a
  type Pos (ListStream a) = Int

  next (ListStream [] _) = Nothing
  next (ListStream (x : xs) p) = Just (x, ListStream xs (p + 1))

  getPos (ListStream _ p) = p
