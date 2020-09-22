module Library where

import Test.QuickCheck

type Person = String
type Book = String    
type Database = [(Person, [Book])]

--functions
books   :: Database -> Person -> [Book]
books   dBase findPerson
    = [book | (person, borrowedBooks) <- dBase, person == findPerson, book <- borrowedBooks]
--borrowers   :: Database -> Book -> [Person]
--borrowed    :: Database -> Book -> Bool
--numBorrowed :: Database -> Person -> Integer
--makeLoan    :: Database -> Person -> Book -> Database
--returnLoan  :: Database -> Person -> Bool -> Database

--test examples
exampleBase :: Database
exampleBase
    = [("Alice", ["Tintin", "Asterix"]), ("Anna", ["Little Woman"]),
        ("Rory", ["Tintin"])]

--test
test_books :: [Book]
test_books = books exampleBase "Ro"