{-|
Module      : FindParts
Description : CLRS - 2.3.7 - Implementation of the "insertion sort" algorithm
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@"Describe a \theta(n lg n)-time algorithm that, given a set S of n integers 
and another integer x, determines whether or not there exist two elements in S 
whose sum is exactly x"@
-}
module FindParts (findParts) where


findParts   :: Int
            -> [Int]
            -> Bool
findParts v [] = False
findParts v [x] = v == x
findParts v x =
    or [binarySearch k x && binarySearch (v - k) x | k <- [1..v]]


-- | Function to implement merge sort with \theta(log n) complexity
mergeSort :: (Eq a, Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x =
    merge (mergeSort leftHalf) (mergeSort rightHalf)
    where
        splitPoint = floor (fromIntegral (length x) / 2)
        (leftHalf, rightHalf) = splitAt splitPoint x


-- | Helper function to implement merge operation
merge :: (Eq a, Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge left@(x:xs) right@(y:ys)
    | x < y = x : merge xs right
    | otherwise = y : merge left ys


-- | Function to implement binary search to search in \theta(log n) time
binarySearch :: (Eq a, Ord a) => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch v [x] = v == x
binarySearch v x
    | v < middle = binarySearch v left
    | v > middle = binarySearch v right
    | otherwise = True
    where
        splitPoint = floor (fromIntegral (length x) / 2)
        left = take (splitPoint - 1) x
        right = drop splitPoint x
        middle = x !! (splitPoint - 1)
    