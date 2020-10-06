type Poly = [Integer]

--purpose: calculate the sum of 2 polynomials
--parameter: two polynomials
--return value: a list with the sum coefficients of 2 input polynomials
addpoly ::  Poly -> Poly -> Poly
addpoly xs [] = xs
addpoly [] ys = ys
addpoly (x:xs) (y:ys) = (x + y) : (addpoly xs ys)

--purpose: calculate the products of 2 polynomials
--parameter: two polynomials
--return value: a list with the product of 2 polynomials
multpoly :: Poly -> Poly -> Poly
multpoly [] _ = []
multpoly _ [] = []
multpoly (x:xs) ps = addpoly (mul x ps) (multpoly xs (0:ps))

----------other functions------------- 
--purpose: calculate the product of an interger and a polynomial
--parameter: an intenger; a polynomial
--return value: the product of the input intenger and the input polynomial 
mul :: Integer -> Poly -> Poly
mul i [] = []
mul i (p:ps) = (i * p) : (mul i ps)