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