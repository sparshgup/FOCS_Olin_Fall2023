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
  error "Not implemented"


setSub :: [Int] -> [Int] -> Bool
setSub xs ys =
  error "Not implemented"


setEqual :: [Int] -> [Int] -> Bool
setEqual xs ys =
  error "Not implemented"


setUnion :: [Int] -> [Int] -> [Int]
setUnion xs ys =
  error "Not implemented"


setInter :: [Int] -> [Int] -> [Int]
setInter xs ys =
  error "Not implemented"


{- QUESTION 2 -}

mapFunctions :: [a -> b] -> a -> [b]
mapFunctions fs x =
  error "Not implemented"


keepPalindromes :: String -> [String] -> [String]
keepPalindromes s xs =
  error "Not implemented"


incrementPositive :: [Int] -> [Int]
incrementPositive xs =
  error "Not implemented"


distribute :: a -> [b] -> [(a, b)]
distribute x ys =
  error "Not implemented"


consAll :: a -> [[a]] -> [[a]]
consAll x xss =
  error "Not implemented"


{- QUESTION 3 -}

hasFinal :: [Int] -> [Int] -> Bool
hasFinal final qs =
  error "Not implemented"


followSymbol :: [(Int, Char, Int)] -> [Int] -> Char -> [Int]
followSymbol delta qs sym =
  error "Not implemented"


followString :: [(Int, Char, Int)] -> [Int] -> [Char] -> [Int]
followString delta qs syms =
  error "Not implemented"


accept :: FA -> String -> Bool
accept m input =
  error "Not implemented"



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
