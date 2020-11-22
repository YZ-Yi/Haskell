--Yuzhe Yi
--30105971

--purpose: check if the input character is an operator
--parameter: an Char
--return value: bool
isOp :: Char -> Bool
isOp c = (c == '+' || c == '-' || c == '*' || c == '/'|| c =='%')

--purpose: transform the input Char operator to Ops operator
--parameter: an Char
--precondition: input Char is an operator
--return value: an Ops operator
charToOp :: Char -> Ops
charToOp c 
     | c == '+'     = Add
     | c == '-'     = Sub
     | c == '*'     = Mul
     | c == '/'     = Div
     | c == '%'     = Mod

--purpose: make input value to an Expr
--return value: an Expr expression
makeExpr :: (Char, (Expr, (Char, (Expr, Char)))) -> Expr
makeExpr ('(', (e1, (op, (e2, ')')))) = Op o e1 e2
     where o = charToOp op

--purpose: recognize such an object oppitionally
--parameter: a Parse and input value
optional :: Parse a b -> Parse a [b]
optional p  = alt (succeed []) (build p (\x-> [x]))

--purpose: recogize a non-empty list of the onjects
--parameter: a Parse and input value
neList :: Parse a b -> Parse a [b]
neList p = (build (sqn p (list p)) (uncurry (:)))
     
--purpose: transform the input string to an Expr expression
--parameter: a string
--return value: an Expr expression
stringToExpr :: [Char] -> Expr
stringToExpr (x:xs)
     | x == '~'     = Lit (-1 * (read xs))
     | otherwise    = Lit (read (x:xs))
