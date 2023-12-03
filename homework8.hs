{-

--------------------------------------------------

HOMEWORK 8

Due: Sun, Dec 10, 2023 (23h59)

Name:

Email:

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


{- Primitive constructors for dataflow networks. -}

cst :: a -> [a]
cst k = k : (cst k)

fby :: [a] -> [a] -> [a]
fby s t = (head s) : t

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f s t = f (head s) (head t) : map2 f (tail s) (tail t)

drop :: [a] -> [a]
drop s = tail s


{- Sample streams and stream transformers from class. -}

from :: Int -> [Int]
from k = k : from (k + 1)

add1 :: [Int] -> [Int]
add1 s = map (\x -> x + 1) s

add :: [Int] -> [Int] -> [Int]
add s t = map2 (\x y -> x + y) s t

nats :: [Int]
nats = from 0

evens :: [Int]
evens = map (\x -> x * 2) nats

odds :: [Int]
odds = add1 evens

triangles :: [Int]
triangles = 0 : add (add1 nats) triangles

fib :: [Int]
fib = 0 : 1 : add fib (tail fib)

psums :: [Int] -> [Int]
psums s = (head s) : add (tail s) (psums s)

primes :: [Int]
primes =
  let notdivides k n = n `mod` k /= 0
      sieve s = head s : sieve (filter (notdivides (head s)) (tail s))
  in sieve (from 2)


{- Function to get the nth element of a stream. -}

nth :: Int -> [a] -> a
nth 0 s = head s
nth n s = nth (n - 1) (tail s)


{- Test functions/streams. -}

-- [0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, 0, 1, 2, 3, ...]
mod7 :: [Int]
mod7 = map (\x -> x `mod` 7) nats

-- Polynomial defining the square root of v.
psqrt v x = x * x - v
-- Derivative of the polynomial defining the square root of v.
dsqrt v x = 2 * x




{- QUESTION 1 -}

mult :: [Int] -> [Int] -> [Int]
mult s t =
  error "not implemented"


mult2 :: [Int] -> [Int] -> [Int]
mult2 s t =
  error "not implemented"


stutter :: [a] -> [a]
stutter s =
  error "not implemented"


stairs :: [a] -> [a]
stairs s =
  error "not implemented"


running_max :: [Int] -> [Int]
running_max s =
  error "not implemented"



{- QUESTION 2 -}

-- Helper stream transformers for partial sums of floats
psumsf :: [Double] -> [Double]
psumsf s = (head s) : map2 (\x y -> x + y) (tail s) (psumsf s)


spi :: [Double]
spi =
  error "not implemented"


newton :: (Double -> Double) -> (Double -> Double) -> Double -> [Double]
newton f df guess =
  error "not implemented"


derivative :: (Double -> Double) -> Double -> [Double]
derivative f x =
  error "not implemented"


limit :: Double -> [Double] -> [Double]
limit epsilon s =
  error "not implemented"
