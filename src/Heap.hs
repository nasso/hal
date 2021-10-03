module Heap
  ( Heap,
    empty,
    store,
    alloc,
    free,
    fetch,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- | Efficient map from random ints to values of type @a@.
data Heap a = Heap
  { values :: IntMap a,
    freedSlots :: [Int]
  }
  deriving (Eq, Show)

-- | An empty heap.
empty :: Heap a
empty = Heap IntMap.empty []

-- | Store a value in the heap.
store :: Int -> a -> Heap a -> Heap a
store key value heap = heap {values = IntMap.insert key value (values heap)}

-- | Allocate a new slot in the heap.
alloc :: a -> Heap a -> (Int, Heap a)
alloc v h =
  case freedSlots h of
    (k : ks) -> (k, h {values = IntMap.insert k v (values h), freedSlots = ks})
    [] ->
      let k = IntMap.size (values h)
       in (k, h {values = IntMap.insert k v (values h)})

-- | Free a slot in the heap.
free :: Int -> Heap a -> Heap a
free k h =
  h
    { values = IntMap.delete k (values h),
      freedSlots = k : freedSlots h
    }

-- | Fetch a value from the heap.
fetch :: Int -> Heap a -> Maybe a
fetch k h = IntMap.lookup k (values h)
