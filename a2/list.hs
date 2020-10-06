--purpose: merge 2 sorted lists in sorted oder
--parameter: two sorted lists
--prediction: two lists are both in the same sorted order
--return value: a sorted list containing all the elements in the input lists
mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
    | x <= y        = x : (mergeLists xs (y:ys))
    | otherwise     = y : (mergeLists (x:xs) ys)

--purpose: split the input list into a list with element at even index and a list with element at odd index
--parameter: a list
--prediction: indices are zero based
--return value: a pair of 2 list that one contains element at even index and one contains element at odd index
splitList :: [Integer] -> ([Integer], [Integer])
splitList xs = ((evenList xs), (oddList xs))

--purpose: sort the input list by merge sort
--parameter: a list
--return value: a sorted list
mSort :: [Integer] -> [Integer]
mSort [] = []
mSort (x:[]) = [x]
mSort xs = mergeLists (mSort (fst lP)) (mSort (snd lP))
    where lP = splitList xs

----------other functions-------------  
--purpose: select elements from input list at even index
--parameter: a list
--prediction: indices are zero based
--return value: a list containing elements at even index from the input list
evenList :: [Integer] -> [Integer]
evenList [] = []
evenList (x:[]) = x:[]
evenList (x:_:xs) = x : (evenList xs)

--purpose: select elements fron input list at odd index
--parameter: a list
--prediction: indices are zero based
-- return value: a list containing elements at odd index from the input list
oddList :: [Integer] -> [Integer]
oddList [] = []
oddList (x:[]) = []
oddList (x1:x2:xs) = x2 : (oddList xs)