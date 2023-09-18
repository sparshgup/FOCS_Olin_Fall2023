
----------------------------------------
-- HASKELL:
--
-- Expressions
-- Static types
-- Type inference
-- 
-- Purely functional [no side effects like mutation or output]
-- Very refined type system


----------------------------------------
-- BASIC TYPES:
--
-- Int
-- Float
-- Bool
-- Char
-- String


----------------------------------------
-- BASIC EXPRESSIONS:
--
-- An expression *evaluates* to a value
--
-- literals
--   10
--   3.1419
--   True
--   False
--   "hello world"
--
-- infix operations
--   1 + 2
--   5 * 2.5
--   True && False
--   "mork" ++ "mindy"
--
-- function application
--   min 10 11
--   round 3.141592
--   length "riccardo"
--
-- temporary aliases
--   let x = 10 in x * 2
--
-- conditionals
--   if x == 0 then "zero" else "non-zero"
--
-- pattern matching (see later)
--   case exp of ... 
--
-- ANY expression can be wrapped in parentheses to indicate
-- precedence (cf. mathematical expressions)


----------------------------------------
-- HASKELL INTERACTIVE SHELL (ghci):
--
-- Lets you evaluate expressions and show you the result.
-- Lets you define names and functions via declarations.


----------------------------------------
-- DECLARATIONS
--
-- Defining new names (aliases) or functions.
-- A "program" is a sequence of declarations.

euler :: Float
euler = 2.71828

perimeter :: Float -> Float
perimeter r =
  2 * pi * r

sumSquares :: Float -> Float -> Float
sumSquares x y = (x * x) + (y * y)

-- You can also enter the above directly in ghci, without the type annotations.
-- Haskell will generally infer the correct type.
-- It will often be more general than you want. (Type system IS very refined.)
-- Which is why I recommend you write functions in a file with annotations and
-- :load them into ghci.

-- There are no variables.
-- You do not "change" the value of a name, you hide the old value of the name.
-- Existing references to the name still refer to the old value.
--
--   me = "Riccardo"
--   greetMe x = x ++ me
--   me = "Victoria"

------------------------------------------------------------
-- STRUCTURED DATA
------------------------------------------------------------

----------------------------------------
-- TUPLES:
--
-- Fixed size, arbitrary type for elements.
--
-- Create tuples:
--   (10, "hello", True)           :: (Int, String, Bool)
-- 
-- Access elements of a tuple by pattern matching:
--   case t of (a, b, c) -> ...expression using a, b, c...
--

sumPair :: (Int, Int) -> Int
sumPair p = case p of (x, y) -> x + y

first :: (a, b) -> a
first p = case p of (x, y) -> x

second :: (a, b) -> b
second p = case p of (x, y) -> y

-- Note the "polymorphism" of first and second.
--   test p = (first p) + (1 :: Int)


----------------------------------------
-- LINKED LISTS:
--
-- Arbitrary size, uniform type for elements.
--
-- Create:
--   [1, 2, 3, 4, 5, 6]       :: [Int]
--   ["hello", "world"]       :: [String]
--   []                       :: [a]
--
-- Operation : to combine a item with a list of items
--   1 : []           ==  [1]
--   1 : [2, 3, 4]    ==  [1, 2, 3, 4]
--   Order is important:  <item> : <list>
--   Always puts the item in front of the resulting list.
--
-- Accessing elements of a list
--   head [1, 2, 3, 4]  == 1
--   tail [1, 2, 3, 4]  == [2, 3, 4]
--
-- Accessing elements of a list using pattern matchin
--   case lst of
--     [] -> ...when lst is empty...
--     x : y -> ...x is the head, y is the tail...
-- 
-- Writing functions over list using RECURSION

headWithDef :: [Int] -> Int -> Int
headWithDef lst n =
  case lst of
    [] -> n
    hd : tl -> hd

sumList :: [Int] -> Int
sumList xs =
  case xs of
    [] -> 0
    x : xs' -> x + sumList xs'


-- RECIPE FOR RECURSIVE FUNCTIONS OVER LISTS
--
-- 1. Determine the input of the function you want to loop over
-- 2. Do a case analysis on that input (empty versus not empty)
-- 3. Determine what the answer should be when the input is empty
--    - Return that for the [] case
-- 4. Determine how you can combine the first element of the list and the
--    result of applying the function to the rest of the list to get the result
--    you know you should get (you can work with an example to help you here).
--    - Use what you learn to create the non-empty case
--
-- f xs =
--   case xs of
--     [] -> ...what to do when list is empty...
--     x : xs' -> ...combine x and (f xs') to get the result for xs
--

lengthList :: [a] -> Int
lengthList xs =
  case xs of
    [] -> 0
    x : xs' -> 1 + length xs'

doubleList :: [Int] -> [Int]
doubleList xs =
  case xs of
    [] -> []
    x : xs' -> (2 * x) : doubleList xs'

{-
stutterList :: [Int] -> [Int]
stutterList xs =
-}

