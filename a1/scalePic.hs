module ScalePic where

type Picture = [[Char]]

scale   :: Picture -> Int -> Picture
scale   pic size 
    = scaleStr (scaleLine pic size) size


-- other functions
scalePix    :: [Char] -> Int -> [Char]
scalePix     str s
    = concat (map (replicate s) str)

scaleLine   :: Picture -> Int -> Picture
scaleLine   pic size
    = [scalePix ln size | ln <- pic]


scaleStr    :: Picture -> Int -> Picture
scaleStr    pic size
    = concat (map (replicate size) pic)

-- test 
examplePic  :: Picture
examplePic  = ["#.#", "..#"]
test    :: Picture
test    = scale examplePic 2

--purpose: repeat one pixel certain times
--parameter: c: the input pixel; s: the times we want to scale the pixel
--return value: a list of pixel after repeating the input pixel
repeatPix   :: Char -> Int -> [Char]
repeatPix   c s
    | s == 0    = []
    | s > 0     = c : (repeatPix c (s - 1))

--purpose: scale one row of pixels
--parameter: str: the input pixels; s: the times we want to scale the pixels
--return value: a new row of pixels after scaling the input pixels
scalePix    :: [Char] -> Int -> [Char]
scalePix     str s
    | str == [] = []
    | otherwise = (repeatPix (head str) s) ++ (scalePix (tail str) s)

--purpose: scale the picture horizontally
--parameter: pic: the input picture; size: the times we want to scale the picture
--return value: the new picture after scaling the input picture horizontally
scaleLine   :: Picture -> Int -> Picture
scaleLine   pic size
    = [scalePix ln size | ln <- pic]

--purpose: repeat one row of pixels certain times
--parameter: str: the input row of pixels; size: the times we want to repeate the pixels
--return value: a picture after repeating the input row of pixels
repeatStr   :: [Char] -> Int -> [[Char]]
repeatStr   str size
    | size == 0 = []
    | size > 0  = str : (repeatStr str (size - 1))

--purpose: scale the picture vertically
--parameter: pic: the input picture; size: the times we want to scale the picture
--return value: a new picture after we scale the input picture vertically
scaleStr    :: Picture -> Int -> Picture
scaleStr    pic size
    | pic == [] = []
    | otherwise = (repeatStr (head pic) size) ++ (scaleStr (tail pic) size)
