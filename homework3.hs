{-

--------------------------------------------------

HOMEWORK 3

Due: Sun, Oct 15, 2023 (23h59)

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

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use putStrLn" #-}

{-
 -  The type of a finite automaton
 -
 -}

data FA = FA {
  states :: [Int],
  alphabet :: [Char],
  delta :: [(Int, Char, Int)],
  start :: Int,
  final :: [Int]
}


{-
 - Sample Finite Automata
 -
 - The first is a deterministic finite automaton that accepts the language of all
 - strings over {a,b,c} that start with a and end with c.
 -
 - The second is a nondeterministic finite automaton that accepts the language of all
 - strings over {a,b,c} that end with 'abc'.
 -
 -}

faStartaEndc :: FA
faStartaEndc = FA { states = [0, 1, 2, 99],
                    alphabet = ['a', 'b', 'c'],
                    delta = [ (0, 'a', 1),
                              (1, 'a', 1),
                              (2, 'a', 1),
                              (99, 'a', 99),
                              (0, 'b', 99),
                              (1, 'b', 1),
                              (2, 'b', 1),
                              (99, 'b', 99),
                              (0, 'c', 99),
                              (1, 'c', 2),
                              (2, 'c', 2),
                              (99, 'c', 99) ],
                    start = 0,
                    final = [2]
                  }

faLastabc :: FA
faLastabc = FA { states = [0, 1, 2, 3],
    alphabet = ['a', 'b', 'c'],
    delta = [(0, 'a', 0),
              (0, 'b', 0),
              (0, 'c', 0),
              (0, 'a', 1),
              (1, 'b', 2),
              (2, 'c', 3)],
    start = 0,
    final = [3]
  }



{- QUESTION 1 -}


set :: [Int] -> [Int]
set xs =
  case xs of
    [] -> []
    (x:xs')
      | x `elem` xs' -> set xs'
      | otherwise    -> x : set xs'


setSub :: [Int] -> [Int] -> Bool
setSub xs ys =
  case xs of
    []      -> True
    (x:xs') -> (x `elem` ys) && setSub xs' ys


setEqual :: [Int] -> [Int] -> Bool
setEqual xs ys = setSub xs ys && setSub ys xs


setUnion :: [Int] -> [Int] -> [Int]
setUnion xs ys =
  case xs of
    []      -> set ys
    (x:xs') -> if x `elem` ys
               then setUnion xs' ys
               else x : setUnion xs' ys



setInter :: [Int] -> [Int] -> [Int]
setInter xs ys =
  case xs of
    [] -> []
    (x:xs') -> if x `elem` ys
               then x : setInter xs' ys
               else setInter xs' ys

{- QUESTION 2 -}

mapFunctions :: [a -> b] -> a -> [b]
mapFunctions fs x = map ($ x) fs


keepPalindromes :: String -> [String] -> [String]
keepPalindromes s xs = filter (\x -> s ++ x == reverse (s ++ x)) xs


incrementPositive :: [Int] -> [Int]
incrementPositive xs = map (\x -> if x > 0 then x + 1 else x) (filter (> 0) xs)


distribute :: a -> [b] -> [(a, b)]
distribute x ys = map (\y -> (x, y)) ys


consAll :: a -> [[a]] -> [[a]]
consAll x xss = map (x :) xss


{- QUESTION 3 -}

hasFinal :: [Int] -> [Int] -> Bool
hasFinal final qs = any (`elem` final) qs


followSymbol :: [(Int, Char, Int)] -> [Int] -> Char -> [Int]
followSymbol delta qs sym =
  concatMap (\q -> followSymbolFromState delta q sym) qs

{- Helper function for followSymbol -}
followSymbolFromState :: [(Int, Char, Int)] -> Int -> Char -> [Int]
followSymbolFromState delta q sym =
  [s | (q', sym', s) <- delta, q' == q, sym' == sym]


followString :: [(Int, Char, Int)] -> [Int] -> [Char] -> [Int]
followString delta qs [] = qs
followString delta qs (sym:syms) =
  let nextStates = followSymbol delta qs sym
  in followString delta nextStates syms


accept :: FA -> String -> Bool
accept m input = hasFinal (final m) (followString (delta m) [start m] input)



{-

This function is the base function that basically loops through all
strings  of length up to n, and prints those that are accepted by the
finite automaton.

This is being way too clever to try to not blow the stack
while enumerating all strings up to a given length. Basically.
we enumerate all integer, convert them to base K (where K is the
size of the alphabet) and then replace every digit base K by the
letter of the alphabet at the corresponding index in the alphabet.

The key is that we can enumerate integers super easily

-}

lang :: FA -> Int -> IO ()
lang m n =

  let take n def l =
        if n <= 0
          then []
        else case l of
               [] -> def : take (n - 1) def l
               x:xs -> x : take (n - 1) def xs

      nth xs n =
        case xs of
          [] -> error "list too short"
          x:xs' -> if n == 0 then x
                   else nth xs' (n - 1)

      to_base_n base size n =
        let loop n =
              if n <= 0 then []
              else if mod n base == 0 then 0 : (loop (div n base))
              else (mod n base) : (loop (div (n - mod n base) base))  in
        take size 0 (loop n)

      to_string alphabet size n =
        let base = length alphabet
            num_base = to_base_n base size n in
        map (\i -> nth alphabet i) num_base

      print_str s =
        if s == ""
          then putStr "  <empty-string>\n"
        else putStr ("  " ++ s ++ "\n")

      print_loop i =
        if i <= n then
          let ts = to_string (alphabet m) i
              bound = (length (alphabet m)) ^ i
              loop2 j =
                if j < bound then (if accept m (ts j)
                                     then print_str (ts j)
                                   else return ()) >>
                                   loop2 (j + 1)
                else return () in
          loop2 0 >> print_loop (i + 1)
        else return ()  in

  if n < 0
    then return ()
  else
    print_loop 0
