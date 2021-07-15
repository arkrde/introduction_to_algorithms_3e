{-|
Module      : MaxSubArray
Description : CLRS - 4.1 - The Maximum-subarray Problem
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@"A solution using divide-and-conquer"@
-}
module MaxSubArray 
    ( maxSubArray
    ) where

import qualified Data.List as List

-- | The `maxSubArray` function returns the maximum subarray from
-- within an array
maxSubArray :: (Num a, Ord a) 
            => [a] -- ^ The input array
            -> [a] -- ^ The maximum subarray
maxSubArray [] = []
maxSubArray [x] = [x]
maxSubArray xs = 
    List.foldl1' (\acc ys -> if sum ys > sum acc then ys else acc) 
        $ [maxSubArray left, maxSubArray right, cross]
    where
        splitPoint = floor (fromIntegral (length xs) / 2)
        (left, right) = splitAt splitPoint xs
        cross = findMaxCrossingSubarray left right

-- | Function to determine maximum subarray that crosses over from 
-- from the first argument to the second argument
findMaxCrossingSubarray :: (Num a, Ord a) => [a] -> [a] -> [a]
findMaxCrossingSubarray left right =
    maxLeftSub ++ maxRightSub
    where
        maxRightSub = findMaxSubFromLeft right
        maxLeftSub = reverse . findMaxSubFromLeft $ reverse left
        
        --maxLeftSub = findMaxSubFromRight left
        --maxRightSub = reverse . findMaxSubFromRight $ reverse right

-- | Function to determine the maximum subarray within an array
-- by scanning from the right
findMaxSubFromRight :: (Num a, Ord a) => [a] -> [a]
findMaxSubFromRight xs =
    foldr1 (\xs acc -> if sum acc < sum xs then xs else acc) 
        $ init $ scanr (:) [] xs


findMaxSubFromLeft :: (Num a, Ord a) => [a] -> [a]
findMaxSubFromLeft xs =
    List.foldl1' (\acc xs -> if sum xs > sum acc then xs else acc) 
        $ tail 
        $ map reverse 
        $ List.scanl' (flip (:)) [] xs

