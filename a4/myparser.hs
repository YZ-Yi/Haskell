--Yuzhe Yi
--30105971
isOp :: Char -> Bool
isOp c = (c == '+' || c == '-' || c == '*' || c == '/'|| c =='%')

charToOp :: Char -> Ops
charToOp c 
     | c == '+'     = Add
     | c == '-'     = Sub
     | c == '*'     = Mul
     | c == '/'     = Div
     | c == '%'     = Mod

makeExpr :: (Char, (Expr, (Char, (Expr, Char)))) -> Expr
makeExpr ('(', (e1, (op, (e2, ')')))) = Op o e1 e2
     where o = charToOp op

optional :: Parse a b -> Parse a [b]
optional p inp 
     | null t       = ls
     | otherwise    = [head ls] ++ [head t]
     where 
          ls = list p inp
          t = tail ls

neList :: Parse a b -> Parse a [b]
neList p inp 
     | null t       = ls   
     | otherwise    = [head (tail ls)] ++ (tail ls)
     where 
          ls = list p inp
          t = tail ls
     
stringToExpr :: [Char] -> Expr
--stringToExpr [] = Lit 0
stringToExpr (x:xs)
     | x == '~'     = Lit (-1 * (read xs))
     | otherwise    = Lit (read (x:xs))
