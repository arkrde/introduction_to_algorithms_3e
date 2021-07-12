module BinaryAddition (binaryAddition) where

-- | Helper function to calculate the carry from the bitwise
-- addition of two binary arrays
binaryCarry :: [Bool] -> [Bool] -> Bool
binaryCarry [] [] = False -- TODO: just boilerplate
binaryCarry (x:xs) [] = False -- TODO: just boilerplate
binaryCarry [] (x:xs) = False -- TODO: just boilerplate
binaryCarry [x] [y] = x && y
binaryCarry (x:xs) (y:ys)
    | carry = x || y
    | otherwise = x && y
    where
        carry = binaryCarry xs ys

-- | Helper function to calculate the result of bitwise addition
-- of two binary arrays
binarySum :: [Bool] -> [Bool] -> [Bool]
binarySum [x] [y] = [(x || y) && not (x && y)]
binarySum (x:xs) (y:ys) 
    | carry == False = ((x || y) && not (x && y)) : (binarySum xs ys)
    | otherwise = ((x && y) || ((not x) && (not y))) : (binarySum xs ys)
    where
        carry = binaryCarry xs ys

-- | Function to implement bitwise addition of two binary arrays
binaryAddition  :: [Bool] -- ^ First operand
                -> [Bool] -- ^ Second operand
                -> [Bool] -- ^ Result
binaryAddition x y
    | carry = carry : sum
    | otherwise = sum
    where 
        carry = binaryCarry x y
        sum = binarySum x y
