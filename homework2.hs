{-

--------------------------------------------------

HOMEWORK 2

Due: Thu, Oct 5, 2023 (23h59)

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
import GHC.Exts.Heap (GenClosure(fun))
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use putStrLn" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}


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
 - The first accepts the language of all strings over {a,b}
 - with a multiple-of-3 number of a's.
 -
 - The second accepts the language of all strings over {a,b,c}
 - that start with a and end with c.
 -
 -}

faMod3a :: FA
faMod3a = FA { states = [1, 2, 3],
               alphabet = ['a', 'b'],
               delta = [ (1,'a',2),
                         (2,'a',3),
                         (3,'a',1),
                         (1,'b',1),
                         (2,'b',2),
                         (3,'b',3) ],
               start = 1,
               final = [1]
             }

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



{- QUESTION 1 -}


isFinal :: [Int] -> Int -> Bool
isFinal xs s = s `elem` xs


followSymbol :: [(Int, Char, Int)] -> Int -> Char -> Int
followSymbol [] _ _ = error "incomplete automaton"
followSymbol ((q1, sym1, q2):trs) q sym
  | q == q1 && sym == sym1 = q2 
  | otherwise = followSymbol trs q sym


followString :: [(Int, Char, Int)] -> Int -> [Char] -> Int
followString [] _ [] = error "incomplete automaton"
followString trs q [] = q
followString trs q (s:syms) =
  let nextState = followSymbol trs q s
  in followString trs nextState syms
  

accept :: FA -> String -> Bool
accept m input = isFinal (final m) (followString (delta m) (start m) input)



{- QUESTION 2 -}

dummy :: FA
dummy = FA { states = [0],
             alphabet = ['x'],
             delta = [(0, 'x', 0)],
             start = 0,
             final = []
           }

fa_part_a :: FA
fa_part_a = FA { states = [0, 1, 2],
                 alphabet = ['a', 'b', 'c'],
                 delta = [(0, 'a', 1),
                          (0, 'b', 1),
                          (0, 'c', 1),
                          (1, 'a', 2),
                          (1, 'b', 2),
                          (1, 'c', 2),
                          (2, 'a', 0),
                          (2, 'b', 0),
                          (2, 'c', 0)],
                 start = 0,
                 final = [0, 1]
               }


fa_part_b :: FA
fa_part_b = FA { states = [0, 1, 2, 3, 4, 5],
                 alphabet = ['a', 'b', 'c'],
                 delta = [(0, 'a', 1),
                          (0, 'b', 5),
                          (0, 'c', 0),
                          (1, 'a', 2),
                          (1, 'b', 5),
                          (1, 'c', 1),
                          (2, 'a', 3),
                          (2, 'b', 5),
                          (2, 'c', 2),
                          (3, 'a', 4),
                          (3, 'b', 5),
                          (3, 'c', 3),
                          (4, 'a', 5),
                          (4, 'b', 5),
                          (4, 'c', 4),
                          (5, 'a', 5),
                          (5, 'b', 5),
                          (5, 'c', 5)],
                 start = 0,
                 final = [4]
               }


fa_part_c :: FA
fa_part_c = FA { states = [0, 1, 2, 3, 4, 5, 6, 7, 8],
                 alphabet = ['a', 'b', 'c'],
                 delta = [(0, 'a', 1),
                          (0, 'b', 4),
                          (0, 'c', 0),
                          (1, 'a', 2),
                          (1, 'b', 5),
                          (1, 'c', 1),
                          (2, 'a', 3),
                          (2, 'b', 6),
                          (2, 'c', 2),
                          (3, 'a', 8),
                          (3, 'b', 7),
                          (3, 'c', 3),
                          (4, 'a', 5),
                          (4, 'b', 8),
                          (4, 'c', 4),
                          (5, 'a', 6),
                          (5, 'b', 8),
                          (5, 'c', 5),
                          (6, 'a', 7),
                          (6, 'b', 8),
                          (6, 'c', 6),
                          (7, 'a', 8),
                          (7, 'b', 8),
                          (7, 'c', 7),
                          (8, 'a', 8),
                          (8, 'b', 8),
                          (8, 'c', 8)],
                 start = 0,
                 final = [7]
               }


fa_part_d :: FA
fa_part_d = FA { states = [0, 1, 2, 3, 4],
                 alphabet = ['a', 'b', 'c'],
                 delta = [(0, 'a', 1),
                          (0, 'b', 2),
                          (0, 'c', 0),
                          (1, 'a', 0),
                          (1, 'b', 3),
                          (1, 'c', 1),
                          (2, 'a', 3),
                          (2, 'b', 0),
                          (2, 'c', 2),
                          (3, 'a', 4),
                          (3, 'b', 1),
                          (3, 'c', 3),
                          (4, 'a', 3),
                          (4, 'b', 0),
                          (4, 'c', 4)],
                 start = 0,
                 final = [1]
               }


fa_part_e :: FA
fa_part_e = FA { states = [0, 1, 2, 3, 4, 5, 6],
                 alphabet = ['a', 'b', 'c'],
                 delta = [(0, 'a', 1),
                          (0, 'b', 3),
                          (0, 'c', 6),
                          (1, 'a', 0),
                          (1, 'b', 4),
                          (1, 'c', 6),
                          (2, 'a', 5),
                          (2, 'b', 0),
                          (2, 'c', 6),
                          (3, 'a', 4),
                          (3, 'b', 2),
                          (3, 'c', 6),
                          (4, 'a', 3),
                          (4, 'b', 5),
                          (4, 'c', 6),
                          (5, 'a', 2),
                          (5, 'b', 1),
                          (5, 'c', 6),
                          (6, 'a', 6),
                          (6, 'b', 6),
                          (6, 'c', 6)],
                 start = 0,
                 final = [0]
               }



{- QUESTION 3 -}


makeFunction :: [(Int, b)] -> b -> Int -> b
makeFunction ts def a =
  case lookup a ts of
    Nothing -> def 
    Just output -> output
    

functionGraph :: (Int -> b) -> [Int] -> [(Int, b)]
functionGraph _ [] = []
functionGraph f (v:domain) = (v, f v) : functionGraph f domain


  
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
