data Tree a = Empty | Node a (Tree a) (Tree a)

foldTree :: (a -> a -> a -> a) -> a -> Tree a -> a
foldTree f c Empty = c
foldTree f c (Node x l r) = f x (foldTree f c l) (foldTree f c r)

depth' tr = foldTree fun 0 tr
    where fun _ lv rv = 1 + max lv rv

tree = (Node 1
            (Node 2 Empty Empty)
             (Empty)   )