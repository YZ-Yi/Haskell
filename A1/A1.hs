-- name: Yuzhe Yi
-- ucid: 30105971

------------------------------------
----------------Q1------------------
--purpose: compute log_b x
--parameter: b: base; x:  the number to be computetd
--precondition: 2 arguments are both positive, and base(b) is at least 2
--return value: the biggest non-negative integer y such that b^y <= x
myLog :: Integer -> Integer -> Integer
myLog b x
    | b > x     = 0
    | b <= x    = 1 + myLog b (quot x b)

--------------------------------------


--------------------------------------
----------------Q2--------------------
type Person = String
type Book = String    
type Database = [(Person, [Book])]

----------functions-------------------
--purpose: find the books the person has borrowed
--parameter: dBase: a database of the info of borrowd books and the borrowers
--parameter: findPerson: the person who we want to find out the books he/she has borrowed
--return value: the list of books which the person has borrowed 
books   :: Database -> Person -> [Book]
books   dBase findPerson
    = [book | (person, borrowedBooks) <- dBase, person == findPerson, book <- borrowedBooks]

--purpose: find the borrower(s) of the book
--parameter: dBase: a database of the info of borrowd books and the borrowers
--parameter: findBook: the book we want to find out its borrower(s)
--return value: the list of borrowers who has borrowed the book
borrowers   :: Database -> Book -> [Person]
borrowers   dBase findBook
    = [pers | (pers, booklst) <- dBase, book <- booklst, book == findBook]

--purpose: find out whether the book is borrowed
--parameter: dBase: a database of the info of borrowd books and the borrowers
--parameter: findBook: the book we want to find out whether it is borrowed
--return value: a bool value that whether the book is borrowed or not
borrowed    :: Database -> Book -> Bool
borrowed    dBase findBook
    = (borrowers dBase findBook) /= []

--purpose: find out the number of the book(s) the person has borrowed
--parameter: dBase: a database of the info of borrowd books and the borrowers
--parameter: findPerson: the person we want to find out the number of the book(s) he/she has borrowed
--return value: the number of the book(s) the person has borrowed
numBorrowed :: Database -> Person -> Int
numBorrowed dBase findPerson
    = length (books dBase findPerson)

--purpose: add new infomation about the new borrowed book and its borrower
--parameter: dBase: a database of the info of borrowd books and the borrowers
--parameter: findPerson: the person who has borrowed the book
--parameter: findBook: the book that has been borrowed
--precondition: if the person has already checked out the book, the database remains the same
--return value: a new database after the person borrows the book
makeLoan    :: Database -> Person -> Book -> Database
makeLoan    dBase findPerson findBook
    -- if the person has checked the book
    | elem findBook (books dBase findPerson)            = dBase
    -- if the person has not checked any books before
    | (books dBase findPerson) == []                    = (findPerson, [findBook]) : dBase
    | otherwise                                         = (findPerson, findBook : (books dBase findPerson)) : (rmPersonFromDatabase dBase findPerson)

--purpose: remove the information about the borrower and the borrowed returned
--parameter: dBase: a database of the info of borrowd books and the borrowers
--parameter: findPerson: the person who has returned the book
--parameter: findBook: the book that has been returned
--return value: a new database after the person returns the book
returnLoan  :: Database -> Person -> Book -> Database
returnLoan  dBase findPerson findBook
    -- if the person did not borrow any books
    | books dBase findPerson == []                              = dBase
    -- if it is the only book the person borrowed
    | (rmBookFromLst (books dBase findPerson) findBook) == []   = rmPersonFromDatabase dBase findPerson
    | otherwise                                                 = (findPerson, (rmBookFromLst (books dBase findPerson) findBook)) : (rmPersonFromDatabase dBase findPerson)

----------other functions-------------  
--purpose: remove the person from the database if he/she has borrowed no books
--parameter: dBase: a database of the info of borrowd books and the borrowers
--parameter: findPerson: the person who needs to be removed
--prediction: the person is supposed to have borrowed no books
--return value: a new database after the person being removed
rmPersonFromDatabase    :: Database -> Person -> Database
rmPersonFromDatabase    dBase findPerson
    = [(pers, bklst) | (pers, bklst) <- dBase, pers /= findPerson]

--purpose: remove the book from the book list
--parameter: lst: a list of books; element: the book we want to remove from the list
--return value: a new list after the book being removed
rmBookFromLst   :: [Book] -> Book -> [Book]
rmBookFromLst   lst element
    = [el | el <- lst, el /= element]

--------------------------------------


------------------------------------
----------------Q3------------------
type Picture = [[Char]]

--purpose: scale the input picture by the size
--parameter: pic: the input pic; size: the size we want to scale the picture
--return value: the new picture after the input pictuere being scaled
scale   :: Picture -> Int -> Picture
scale   pic size 
    = scaleStr (scaleLine pic size) size

--------------------------------------
-----------other functions------------
--purpose: scale one row of pixels
--parameter: str: the input pixels; s: the times we want to scale the pixels
--return value: a new row of pixels after scaling the input pixels
scalePix    :: [Char] -> Int -> [Char]
scalePix     str s
    = concat (map (replicate s) str)

--purpose: scale the picture horizontally
--parameter: pic: the input picture; size: the times we want to scale the picture
--return value: the new picture after scaling the input picture horizontally
scaleLine   :: Picture -> Int -> Picture
scaleLine   pic size
    = [scalePix ln size | ln <- pic]

--purpose: scale the picture vertically
--parameter: pic: the input picture; size: the times we want to scale the picture
--return value: a new picture after we scale the input picture vertically
scaleStr    :: Picture -> Int -> Picture
scaleStr    pic size
    = concat (map (replicate size) pic)


--------------------------------------
--------------Q4----------------------
type Graph = [(Int, Int)]

--purpose: find the common friends x and y both have
--parameter: x: the first given person; y: the second given person
--return value: a list of friends that x and y both have
commonFriends   :: Graph -> Int -> Int -> [Int]
commonFriends   g x y
    = findCommon (findFriends g x) (findFriends g y)

-------other functions----------------
--purpose: find all friends the given person has
--parameter: g: a social graph; pers: the given person
--return value: a list of friends that the given person has
findFriends  :: Graph -> Int -> [Int]
findFriends g pers
    = [f | (f, p) <- g, p == pers] ++ [f | (p, f) <- g, p == pers]

--purpose: find the common elements of 2 lists
--parameter: aLst: the first given list; bLst: the second given list
--return value: a list of elements that aLst and bLst both have
findCommon  :: [Int] -> [Int] -> [Int]
findCommon  aLst bLst
    = [a | a <- aLst, b <- bLst, a == b]