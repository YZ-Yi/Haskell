module Demo_QuickCheck where

import Test.QuickCheck

square :: Integer -> Integer
square n = n*n
        
double :: Integer -> Integer
double n = 2*n

prop_test1 n = square n == double n

prop_test2 n = square n == (n^2)

