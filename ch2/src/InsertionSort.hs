{-|
Module      : InsertionSort
Description : CLRS - 2.3.4 - Implementation of the "insertion sort" algorithm
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@"We can express insertion sort as a recursive procedure as follows. In order 
to sort A[1..n], we recursively sort A[1..n-1] and then insert A[n] into 
the sorted array A[1..n-1]. Write a recurrence for the running time of this 
recursive version of insertion sort."@
-}
module InsertionSort (insertionSort) where

-- | The `insertionSort` function sorts an unsorted list and returns
-- the sorted list while keeping the original list intact
insertionSort   :: (Eq a, Ord a)
                => [a] -- ^ Unsorted "input" list
                -> [a] -- ^ Sorted "output" list
insertionSort [] = []
insertionSort [x] = [x]
insertionSort x = insertElement (last x) (insertionSort (init x))

-- | Helper function to insert a given element in a list and returns
-- the modified list
insertElement :: Ord a => a -> [a] -> [a]
insertElement e [] = [e]
insertElement e (x : xs)
    | e < x = e : x : xs 
    | otherwise = x : insertElement e xs
