-- name: Yuzhe Yi
-- ucid: 30105971

---------------------------------------------------------Q1-----------------------------------------------------------
data Formula = Variable String| Not Formula | And Formula Formula | Or Formula Formula

-- (b) Not (And (Not p) (Not q))

--purpose: return a string representation of the input formula
--parameter: a formula
--return value: a string representation of the input formula
showFormula :: Formula -> String
showFormula (Variable s) = s
showFormula (Not f) = "~" ++ (showFormula f) ++ ""
showFormula (And f1 f2) = "(" ++ (showFormula f1) ++ " ^ " ++  (showFormula f2) ++ ")"
showFormula (Or f1 f2) = "(" ++ (showFormula f1) ++ " v " ++ (showFormula f2) ++ ")"

--purpose: returns a wff f' formula such that f' is logically  equibalemt to the input formula and f' is in NNF
--parameter: a formula
--return value: a rewrited formula of input formula
rewrite :: Formula -> Formula
rewrite (Not (Not f)) = rewrite f
rewrite (Not (And f1 f2)) = rewrite (Or (Not f1) (Not f2))
rewrite (Not (Or f1 f2)) = rewrite (And (Not f1) (Not f2))
rewrite (And f1 f2) = And (rewrite f1) (rewrite f2)
rewrite (Or f1 f2) = Or (rewrite f1) (rewrite f2)
rewrite (Not f) = Not (rewrite f)
rewrite (Variable f) = Variable f




---------------------------------------------------------Q3-----------------------------------------------------------
--purpose: returns the last element of the list argument
--parameter: a list
--return value: the last element of the input list
lastElm :: [a] -> a
lastElm xs = foldr1 (\l r -> r) xs

--purpose: The function takes a list of pred-icates and an entity as arguments,
--         then applies every predicate to the entity, and finallyreturnsTrueif and only if every predicate in the list returnsTrue.
--parameter: a list of predicates and an entity
--precondition: when the list argument is empty, then True is returned
--return value: True of False
unanimous :: [a -> Bool] -> a -> Bool
unanimous fs x = foldl (&&) True (psToBs fs x)

--purpose: The  func-tion takes three arguments:  The function selectiveMaptests tests every  element  of  the  list  argument  using  the  given  predicate.
--         Those elements that satisfy the predicate will be transformed by the function argument.
--         The transformed elements are then collected into a list, which is returned as the value of selectiveMap
--parameter: (i) a predicate for type-avalues, (ii) a function that trans-forms a type-avalue to a type-bvalue, and (iii) a list of type-avalues.
--return value: a list of elemnts which are the transformed elements of the input list which are satisfied predicate
selectiveMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
selectiveMap f t xs = map t (filter f xs)

------------help functions-----------------
--purpose: apply every predicate to the entity
--parameter: (i) a list of predicates, (ii) an entity
--return value: a list of bool value
psToBs :: [a -> Bool] -> a -> [Bool]
psToBs fs x = map (pToB x) fs

--purpose: apply input predicate to the entity
--parameter: (i) an entity, (ii) a predicate
--return value: a bool value
pToB :: a -> (a -> Bool)-> Bool
pToB x f = (f x)






---------------------------------------------------------Q4-----------------------------------------------------------
--purpose: iterate f function on input for n times
--parameter: (i) an interger for iteration times (ii) a function
--           (iii) an input variable for the input function
--precondition: iter 0 f x returns x
--return value: the value after f occurs n times for x
iter :: Integer -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n - 1) f x)

--purpose: returns the value of the power of two with n as the exponent
--parameter: integer n
--return value: the value of the power of two with n as the exponent
powerOfTwo :: Integer -> Integer
powerOfTwo x = iter x double 1

-------------------help function----------------------------------------
--purpose: double the input integer
--parameter: integer
--return value: the value of the input value get doubled
double :: Integer -> Integer
double x = 2 * x