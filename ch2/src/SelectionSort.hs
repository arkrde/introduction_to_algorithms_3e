module SelectionSort (selectionSort) where

-- |The `selectionSort` function sorts a list using the "selection sort" algorithm
-- and returns the sorted list keeping the original list intact.
selectionSort   :: Ord a 
                => [a] -- ^ The unsorted "input" list
                -> [a] -- ^ The sorted "output" list
selectionSort [] = []
selectionSort [x] = [x]
selectionSort (x:xs)
    | x > minVal = minVal : selectionSort (x : removeElement minVal xs)
    | otherwise = x : selectionSort xs
    where minVal = minimum xs

-- |Helper function to remove an element from a list and return the
-- modified list
removeElement :: Eq a => a -> [a] -> [a]
removeElement val [] = []
removeElement val (x:xs)
    | val == x = xs
    | otherwise = x : removeElement val xs
