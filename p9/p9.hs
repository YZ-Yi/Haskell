nTimes :: Integer -> Parse a b -> Parse a [b]
nTimes 0 p = succeed []
nTimes n p = alt (succeed []) (build (sqn p (nTimes (n - 1) p)) (uncurry (:)))