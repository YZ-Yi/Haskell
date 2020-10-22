data Formula = Variable String| Not Formula | And Formula Formula | Or Formula Formula

-- (b) Not (And (Not p) (Not q))

showFormula :: Formula -> String
showFormula (Variable s) = s
showFormula (Not f) = "~" ++ (showFormula f) ++ ""
showFormula (And f1 f2) = "(" ++ (showFormula f1) ++ " ^ " ++  (showFormula f2) ++ ")"
showFormula (Or f1 f2) = "(" ++ (showFormula f1) ++ " v " ++ (showFormula f2) ++ ")"

rewrite :: Formula -> Formula
rewrite (Not (Not f)) = rewrite f
rewrite (Not (And f1 f2)) = rewrite (Or (Not f1) (Not f2))
rewrite (Not (Or f1 f2)) = rewrite (And (Not f1) (Not f2))
rewrite (And f1 f2) = And (rewrite f1) (rewrite f2)
rewrite (Or f1 f2) = Or (rewrite f1) (rewrite f2)
rewrite (Not f) = Not (rewrite f)
rewrite (Variable f) = Variable f

f :: Formula
f = Not (And (Variable "p") (Or (Not (Not (Variable "q"))) (Not (And (Variable "x") (Not (Variable "z"))))))