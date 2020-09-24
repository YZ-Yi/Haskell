module ScalePic where

type Picture = [[Char]]

scale   :: Picture -> Int -> Picture
scale   pic size 
    = scaleStr (scaleLine pic size) size


-- other functions
repeatPix   :: Char -> Int -> [Char]
repeatPix   c s
    | s == 0    = []
    | s > 0     = c : (repeatPix c (s - 1))

scalePix    :: [Char] -> Int -> [Char]
scalePix     str s
    | str == [] = []
    | otherwise = (repeatPix (head str) s) ++ (scalePix (tail str) s)

scaleLine   :: Picture -> Int -> Picture
scaleLine   pic size
    = [scalePix ln size | ln <- pic]

repeatStr   :: [Char] -> Int -> [[Char]]
repeatStr   str size
    | size == 0 = []
    | size > 0  = str : (repeatStr str (size - 1))

scaleStr    :: Picture -> Int -> Picture
scaleStr    pic size
    | pic == [] = []
    | otherwise = (repeatStr (head pic) size) ++ (scaleStr (tail pic) size)

-- test 
examplePic  :: Picture
examplePic  = ["#.#", "..#"]
test    :: Picture
test    = scale examplePic 2