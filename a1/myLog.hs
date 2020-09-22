module MyLog where

myLog :: Integer -> Integer -> Integer
myLog b x
    | b > x     = 0
    | b <= x    = 1 + myLog b (quot x b)