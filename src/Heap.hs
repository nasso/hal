module Heap
  ( Heap,
    empty,
    store,
    alloc,
    free,
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
store :: Heap a -> Int -> a -> Heap a
store heap key value = heap {values = IntMap.insert key value (values heap)}

-- | Allocate a new slot in the heap.
alloc :: Heap a -> a -> (Int, Heap a)
alloc h v =
  case freedSlots h of
    (k : ks) -> (k, h {values = IntMap.insert k v (values h), freedSlots = ks})
    [] ->
      let k = IntMap.size (values h)
       in (k, h {values = IntMap.insert k v (values h)})

-- | Free a slot in the heap.
free :: Heap a -> Int -> Heap a
free h k =
  h
    { values = IntMap.delete k (values h),
      freedSlots = k : freedSlots h
    }
