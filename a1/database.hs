module Library where

import Test.QuickCheck

type Person = String
type Book = String    
type Database = [(Person, [Book])]

--functions
books   :: Database -> Person -> [Book]
books   dBase findPerson
    = [book | (person, borrowedBooks) <- dBase, person == findPerson, book <- borrowedBooks]

borrowers   :: Database -> Book -> [Person]
borrowers   dBase findBook
    = [pers | (pers, booklst) <- dBase, book <- booklst, book == findBook]

borrowed    :: Database -> Book -> Bool
borrowed    dBase findBook
    = (borrowers dBase findBook) /= []

numBorrowed :: Database -> Person -> Int
numBorrowed dBase findPerson
    = length (books dBase findPerson)

makeLoan    :: Database -> Person -> Book -> Database
makeLoan    dBase findPerson findBook
    -- if the person has checked the book
    | elem findBook (books dBase findPerson)            = dBase
    -- if the person has not checked any books before
    | (books dBase findPerson) == []                    = (findPerson, [findBook]) : dBase
    | otherwise                                         = (findPerson, findBook : (books dBase findPerson)) : (rmPersonFromDatabase dBase findPerson)

returnLoan  :: Database -> Person -> Book -> Database
returnLoan  dBase findPerson findBook
    -- if the person did not borrow any books
    | books dBase findPerson == []                              = dBase
    -- if it is the only book the person borrowed
    | (rmBookFromLst (books dBase findPerson) findBook) == []   = rmPersonFromDatabase dBase findPerson
    | otherwise                                                 = (findPerson, (rmBookFromLst (books dBase findPerson) findBook)) : (rmPersonFromDatabase dBase findPerson)

--other functions
rmPersonFromDatabase    :: Database -> Person -> Database
rmPersonFromDatabase    dBase findPerson
    = [(pers, bklst) | (pers, bklst) <- dBase, pers /= findPerson]

rmBookFromLst    :: [Book] -> Book -> [Book]
rmBookFromLst       lst element
    = [el | el <- lst, el /= element]

--test examples
exampleBase :: Database
exampleBase
    = [("Alice", ["Tintin", "Asterix"]), ("Anna", ["Little Woman"]),
        ("Rory", ["Tintin"])]

--test
test_books  :: [Book]
test_books = books exampleBase "Rory"

test_borrowers  :: [Person]
test_borrowers = borrowers exampleBase "Asterix"

test_borrowed   :: Bool
test_borrowed   = borrowed exampleBase "Tintin"

test_numBorrowed    :: Int
test_numBorrowed    = numBorrowed exampleBase "Rory"

test_loan   :: Database
test_loan   = makeLoan exampleBase "Alice" "Tintin"

test_return :: Database
test_return = returnLoan exampleBase "Alice" "Tintin"
