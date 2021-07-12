{-|
Module      : Horner
Description : CLRS - 2.3 - "Horner's rule" the evaluation of a polynomial
Copyright   : (c) Arnab De, 2021
                  Debomita Saha De, 2021
License     : GPL-3
Maintainer  : arkrde@email.com
Stability   : experimental
Portability : POSIX
@""@
-}
module Horner (horner) where
-- | Function to implement "Horner's rule" to evaluate a polynomial
horner  :: (Num a) 
        => a --  ^ The free variable
        -> [a] -- ^ The list of coefficients
        -> a -- ^ The output
horner v [] = 0
horner v (x:xs) = x + v * horner v xs
