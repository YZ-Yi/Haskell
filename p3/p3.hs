concat  ::     [[a]] -> [a]
concat  [] =   []
concat  (x:xs) = x ++ (concat1 xs)

reverse ::  [a] -> [a]
reverse []  =   []
reverse (x:xs) = (reverse1 xs) ++ [x]