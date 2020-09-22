import Test.QuickCheck

prop_1 :: Integer -> Bool
prop_1 x
    |x >= 1     = True
    |otherwise  = False