{-|
Module      : LinearSearch
Description : CLRS - 2.1.3 - Linear Search
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@""@
-}

module LinearSearch (linearSearch) where

isIn :: Eq a => a -> [a] -> Bool
isIn v [] = False
isIn v [x] = v == x
isIn v (x : xs) = (v == x) || isIn v xs

-- | Function to execute "linear search" for an element in a list
linearSearch    :: Eq a 
                => a -- ^ The element to search 
                -> [a] -- ^ The list in which the search is carried out
                -> Int -- ^ Returns the index of the element if present or -1
linearSearch val [] = -1
linearSearch val (x : xs) = searchValue 0 val (x : xs)

-- | Helper function to search for the index of a value in a list.
-- Returns the index of the value if it's found, else return -1
searchValue :: Eq a => Int -> a -> [a] -> Int
searchValue _ _ [] = -1
searchValue idx val (x : xs)
    | val == x = idx
    | otherwise = searchValue (idx + 1) val xs
