import Prelude hiding (curry)

mulUC :: (Int, Int) -> Int
mulUC (x, y) = x * y

curry :: ((a,b) -> c) -> (a -> b -> c)
curry g x y = g (x, y)

myDo :: Int -> Int
myDo = curry mulUC 2