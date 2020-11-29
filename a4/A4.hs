--Yuzhe yi
--30105971

--------------------------------------Q1-----------------------------------------
data Poly = PConst Int |
            PVar |
            PAdd Poly Poly |
            PMul Poly Poly

--purpose: return the denotation of input polynomial
--parameter: a Poly
--return value: the denotation of input polynomial
compilePoly :: Poly -> (Int -> Int)
compilePoly (PConst n) _ = n
compilePoly PVar x = x
compilePoly (PAdd p1 p2) x = (compilePoly p1 x) + (compilePoly p2 x)
compilePoly (PMul p1 p2) x = (compilePoly p1 x) * (compilePoly p2 x)



--------------------------------------Q1-----------------------------------------
--purpose:: calculate the running sums [0, a0, a0+a1, a0+a1+a2, ...] of a list [a0, a1, a2]
--parameter: an infinite list
--return value: the running sums of the input list
runningSum :: [Int] -> [Int]
runningSum xs = 0 : (func xs 0)

-------------------helper function------------------------
--purpose:: calculate the sums of an input list and a given number
--parameter: an infinite list and a number
func :: [Int] -> Int -> [Int]
func (x:xs) y = (x + y):(func xs (x + y))