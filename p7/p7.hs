import Prelude hiding (map, filter)

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\l r -> (f l):r) [] xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = foldr func [] xs where
    func l r
        | p l = l:r
        | otherwise = r