runningSum :: [Int] -> [Int]
runningSum xs = 0 : (func xs 0)

-------------------helper function------------------------
func :: [Int] -> Int -> [Int]
func (x:xs) y = (x + y):(func xs (x + y))