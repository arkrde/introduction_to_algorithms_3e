{-|
Module      : MergeSort
Description : CLRS - 2.3.1 - Merge sort
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@""@
-}
module MergeSort (mergeSort) where

-- | The `mergeSort` function sorts an unsorted list and returns the sorted
-- list keeping the original list intact 
mergeSort   :: (Eq a, Ord a) 
            => [a] -- ^ The unsorted "input" array
            -> [a] -- ^ The sorted "output" array
mergeSort [x] = [x] 
mergeSort x =
    let 
        splitPoint = floor (fromIntegral (length x) / 2)
        (leftHalf, rightHalf) = splitAt splitPoint x
    in
        merge (mergeSort leftHalf) (mergeSort rightHalf)

-- | Helper function to merge to lists in sorted order
merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge lhs@(x:xs) rhs@(y:ys)
    | x < y = x : merge xs rhs
    | otherwise = y : merge lhs ys
