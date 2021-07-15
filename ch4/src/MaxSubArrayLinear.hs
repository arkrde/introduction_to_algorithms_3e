{-|
Module      : MaxSubArrayLinear
Description : CLRS - 4.1.5 - The Maximum-subarray Problem
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@"A linear-time algorithm"@
-}

module MaxSubArrayLinear (
    maxSubarray
    ) where

import qualified Data.List as List


-- | Function to calculate maximum subarray using an O(n) alorithm
maxSubarray :: [Int] -> [Int]
maxSubarray [] = []
maxSubarray [x] = [x]
maxSubarray seq@(x:xs)
    | x < 0 = maxSubarray xs
    | otherwise = findMaxSumSubarray $ removeNegativeSumSequenceFromLeft seq


-- | Function that removes leading subsequences in a list with a negative sum
removeNegativeSumSequenceFromLeft :: (Num a, Ord a) => [a] -> [a]
removeNegativeSumSequenceFromLeft [] = []
removeNegativeSumSequenceFromLeft [x] = if x < 0 then [] else [x]
removeNegativeSumSequenceFromLeft (xs) =
    reverse 
    $ foldr (\x acc -> if sum (x:acc) < 0 then [] else x:acc) [] 
    $ reverse xs


-- | Function to find subarray with the maximum sum from an array without any
-- leading subsequence with a negative sum
findMaxSumSubarray :: (Num a, Ord a) => [a] -> [a]
findMaxSumSubarray [] = []
findMaxSumSubarray [x] = [x]
findMaxSumSubarray (xs) =
    foldl1 (\acc x -> if sum x > sum acc then x else acc) 
    $ map reverse 
    $ scanr (:) [] 
    $ reverse xs

