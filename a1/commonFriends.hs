module CommonFriends where

type Graph = [(Int, Int)]

commonFriends   :: Graph -> Int -> Int -> [Int]
commonFriends   g x y
    = findCommon (findFriends g x) (findFriends g y)

findFriends  :: Graph -> Int -> [Int]
findFriends g pers
    = [f | (f, p) <- g, p == pers] ++ [f | (p, f) <- g, p == pers]

findCommon  :: [Int] -> [Int] -> [Int]
findCommon  aLst bLst
    = [a | a <- aLst, b <- bLst, a == b]

--test
exampleGraph    :: Graph
exampleGraph    = [(1, 5), (1, 3), (1, 4),
                    (3, 6), (2, 3), (2, 4),
                    (2, 6), (2, 5)]

test    :: [Int]
test     = commonFriends exampleGraph 1 10