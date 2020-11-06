mapAll :: [Int] -> (Int -> Int) -> [Int]
mapAll ls f = map f ls

double :: Int -> Int
double x = 2 * x