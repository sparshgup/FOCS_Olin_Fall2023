{-

--------------------------------------------------

HOMEWORK 1

Due: Thu, Sep 21, 2023 (23h59)

Name: Sparsh Gupta

Email: sgupta1@olin.edu

Remarks, if any:



--------------------------------------------------

Please fill in this file with your solutions and submit it

The functions below are stubs that you should replace with your
own implementation.

PLEASE DO NOT CHANGE THE TYPES IN THE STUBS BELOW.
Doing so risks making it impossible for me to test your code.

Always make sure you can :load this file in a FRESH ghci shell
before submitting it. It has to load without any errors.

--------------------------------------------------

-}



-- QUESTION 1

clamp :: Float -> Float -> Float -> Float
clamp a b c
  | c < min a b = min a b
  | c > max a b = max a b
  | otherwise   = c


interpolate :: Float -> Float -> Float -> Float
interpolate a b c =
  a + (b - a) * c


-- QUESTION 2


pairDouble :: [Int] -> [(Int, Int)]
pairDouble xs = [(x, 2 * x) | x <- xs]


cap :: Int -> [Int] -> [Int]
cap m xs = [min x m | x <- xs]


prefix :: String -> [String] -> [String]
prefix s xs = [s ++ x | x <- xs]


longerThan :: Int -> [String] -> [String]
longerThan n xs = [x | x <- xs, length x >= n]


within :: Int -> Int -> [Int] -> [Int]
within a b xs = [x | x <- xs, a <= x && x <= b]


split :: Int -> [Int] -> ([Int], [Int])
split s xs = 
  case xs of 
    [] -> ([],[])
    (x:xs')
      | x <= s    -> (x : lesser, greater)
      | otherwise -> (lesser, x : greater)
      where
        (lesser, greater) = split s xs'


find :: Int -> [(Int, a)] -> a -> a
find n xs def =
  case xs of
    []           -> def
    ((x, y):xs') -> if n == x then y else find n xs' def


-- QUESTION 3

vScale :: Float -> [Float] -> [Float]
vScale a v
  | length v /= length (map (* a) v) = error "vector lengths do not match."
  | otherwise = map (* a) v


vAdd :: [Float] -> [Float] -> [Float]
vAdd v w =
  case (v, w) of
    ([], [])     -> []
    (x:xs, y:ys) -> x + y : vAdd xs ys
    _            -> error "vector lengths do not match."


vInner :: [Float] -> [Float] -> Float
vInner v w =
  case (v, w) of
    ([], [])     -> 0.0
    (x:xs, y:ys) -> x * y + vInner xs ys
    _            -> error "vector lengths do not match."


vLength :: [Float] -> Float
vLength v = sqrt (sumSquares v)


sumSquares :: [Float] -> Float -- Helper function for vLength 
sumSquares xs =
  case xs of
    []      -> 0.0
    (x:xs') -> x * x +sumSquares xs'


-- QUESTION 4

repeatStr :: String -> Int -> String
repeatStr s n 
  | n <= 0 = ""
  | otherwise = s ++ repeatStr s (n - 1)
  

collatzSeq :: Int -> [Int]
collatzSeq n
  | n <= 0    = []
  | n == 1    = [1]
  | even n    = n : collatzSeq (n `div` 2)
  | otherwise = n : collatzSeq (3 * n + 1)
