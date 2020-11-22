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
