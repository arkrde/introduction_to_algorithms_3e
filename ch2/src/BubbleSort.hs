{-|
Module      : BubbleSort
Description : CLRS - 2.2 - Bubble sort
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@"Bubblesort is a popular, but inefficient, sorting algorithm. It works by 
repeatedly swapping adjacent elements that are out of order "@
-}
module BubbleSort (bubbleSort) where


-- |The `bubbleSort` function sorts a list using the "bubble sort" algorithm
-- and returns the sorted list keeping the original list intact.
bubbleSort  :: (Eq a, Ord a)
            => [a] -- ^ The unsorted "input" list
            -> [a] -- ^ The sorted "output" list
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:xs)
    | x > minVal = minVal : bubbleSort (x:rest)
    | otherwise = x : minVal : bubbleSort rest
    where
        (minVal:rest) = bubbleMin xs


-- |Helper function to "bubble up" the minimum value of a list to its
-- first position
bubbleMin   :: (Eq a, Ord a) 
            => [a]
            -> [a]
bubbleMin [] = []
bubbleMin [x] = [x]
bubbleMin (x:xs)
    | x > minRest = minRest : x : tailRest
    | otherwise = x : rest
    where
        rest = bubbleMin xs
        minRest = head rest
        tailRest = tail rest
