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