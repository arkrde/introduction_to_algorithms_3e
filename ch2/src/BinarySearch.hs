{-|
Module      : InsertionSort
Description : CLRS - 2.3.5 - Implementation of the "binary search" algorithm
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@"Referring back to the searching problem (see Exercise 2.1-3), observe that 
if the sequence A is sorted, we can check the midpoint of the sequence against 
v and eliminate half of the sequence from further consideration. The binary 
search  algorithm repeats this procedure, halving the size of the remaining 
portion of the sequence each time. "@
-}
module BinarySearch where

-- | Function to search for an element in a sorted list
-- using the "binary search" algorithm
binarySearch    :: (Eq a, Ord a) 
                => a 
                -> [a]
                -> Bool
binarySearch _ [] = False
binarySearch val [x] = val == x
binarySearch val x
    | val > middle = binarySearch val right
    | val < middle = binarySearch val left
    | otherwise = True
    where 
        splitPoint = floor (fromIntegral (length x) / 2)
        left = take (splitPoint - 1) x
        right = drop splitPoint x
        middle = x !! (splitPoint -1)
