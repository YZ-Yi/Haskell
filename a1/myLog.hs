module MyLog where

myLog :: Integer -> Integer -> Integer
myLog b x
    | b > x     = 0
    | b <= x    = 1 + myLog b (quot x b)

loga_b :: Integer -> Integer -> Int
loga_b a b
	| b < a 	= 0
	| b >= a 	= 1 + loga_b a (b `div` a)

test :: Int
test = loga_b 2 16

