{-

--------------------------------------------------

HOMEWORK 4

Due: Sun, Oct 22, 2023 (23h59)

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


import Control.Monad

-- Type for deterministic Turing machines.
-- States are integers.

data TM = TM { states :: [Int],
               inputAlpha :: [Char],
               tapeAlpha :: [Char],
               delta :: Int -> Char -> (Int, Char, Int),   -- 1 = Right, -1 = Left
               start :: Int,
               accept :: Int,
               reject :: Int
             }

data Config = Config { state :: Int,
                       tape :: [Char],
                       position :: Int
                     } deriving Show


-- Some sample deterministic Turing machines.
--
-- asbs is the regular language {a^m b^n | m,n >= 0}.
-- anbn is the non-regular language {a^n b^n | n >= 0}.
-- anbncn is the non-regular language {a^n b^n c^n | n >= 0}.

asbs :: TM
asbs = let d sym state =
              case (sym, state) of
                (1, 'a') -> (1, 'a', 1)
                (1, 'b') -> (10, 'b', 1)
                (1, '_') -> (777, '_', 1)
                (10, 'b') -> (10, 'b', 1)
                (10, '_') -> (777, '_', 1)
                (777, 'a') -> (777, 'a', 1)
                (777, 'b') -> (777, 'b', 1)
                (777, '_') -> (777, '_', 1)
                (_, c) -> (666, c, 1)  in
         TM { states = [1, 10, 777, 666],
              inputAlpha = ['a', 'b'],
              tapeAlpha = ['a', 'b', '_'],
              start = 1,
              accept = 777,
              reject = 666,
              delta = d }

anbn :: TM
anbn = let d sym state =
              case (sym, state) of
                (1, 'a') -> (2, 'X', 1)
                (1, '_') -> (777, '_', 1)
                (2, 'a') -> (2, 'a', 1)
                (2, 'Y') -> (2, 'Y', 1)
                (2, 'b') -> (4, 'Y', -1)
                (4, 'Y') -> (4, 'Y', -1)
                (4, 'a') -> (7, 'a', -1)
                (4, 'X') -> (6, 'X', 1)
                (6, 'Y') -> (6, 'Y', 1)
                (6, '_') -> (777, '_', 1)
                (7, 'a') -> (7, 'a', -1)
                (7, 'X') -> (1, 'X', 1)
                (_, c) -> (666, c, 1) in
         TM { states = [1, 2, 4, 6, 7, 777, 666],
              inputAlpha = ['a', 'b'],
              tapeAlpha = ['a', 'b', 'X', 'Y', '_'],
              start = 1,
              accept = 777,
              reject = 666,
              delta = d }


anbncn :: TM
anbncn = let d sym state =
                case (sym, state) of
                  (1, 'a') -> (2, 'X', 1)
                  (1, '_') -> (777, '_', 1)
                  (2, 'a') -> (2, 'a', 1)
                  (2, 'Y') -> (2, 'Y', 1)
                  (2, 'b') -> (3, 'Y', 1)
                  (3, 'b') -> (3, 'b', 1)
                  (3, 'Z') -> (3, 'Z', 1)
                  (3, 'c') -> (4, 'Z', -1)
                  (4, 'Z') -> (4, 'Z', -1)
                  (4, 'Y') -> (5, 'Y', -1)
                  (4, 'b') -> (7, 'b', -1)
                  (5, 'Y') -> (5, 'Y', -1)
                  (5, 'X') -> (6, 'X', 1)
                  (6, 'Y') -> (6, 'Y', 1)
                  (6, 'Z') -> (6, 'Z', 1)
                  (6, '_') -> (777, '_', 1)
                  (7, 'b') -> (7, 'b', -1)
                  (7, 'Y') -> (7, 'Y', -1)
                  (7, 'a') -> (8, 'a', -1)
                  (8, 'a') -> (8, 'a', -1)
                  (8, 'X') -> (1, 'X', 1)
                  (_, c) -> (666, c, 1) in
           TM { states = [1, 2, 3, 4, 5, 6, 7, 8, 666, 777],
                inputAlpha = ['a', 'b', 'c'],
                tapeAlpha = ['a', 'b', 'c', 'X', 'Y', 'Z', '_'],
                start = 1,
                accept = 777,
                reject = 666,
                delta = d }



-- QUESTION 1

startConfig :: TM -> String -> Config
startConfig m input =
  error "Not implemented"


isAcceptConfig :: TM -> Config -> Bool
isAcceptConfig m c =
  error "Not implemented"


isRejectConfig :: TM -> Config -> Bool
isRejectConfig m c =
  error "Not implemented"


getNth :: [a] -> Int -> a
getNth init pos =
  error "Not implemented"


replaceNth :: [a] -> Int -> a -> [a]
replaceNth init pos sym =
  error "Not implemented"


step :: TM -> Config -> Config
step m c =
  error "Not implemented"


-- Functions to "run" a Turing machine on a given string.

printConfig :: TM -> Config -> IO ()
printConfig m c =
  let maxLength a r = max (length (show a)) r
      mw = foldr maxLength 0 (states m)
      padding = max 0 (position c - length (tape c))
      tape' = (tape c) ++ (replicate padding '_') in
    do putStr (take mw (show (state c) ++ replicate mw ' '))
       putStr "  "
       forM_ (zip tape' [1..length tape'])
             (\(a, pos) -> if pos == position c
                           then do putStr "["
                                   putChar a
                                   putStr "]"
                           else do putStr " "
                                   putChar a
                                   putStr " ")
       putStrLn ""

validateStates :: TM -> IO ()
validateStates m =
  let addTransitions q syms queue =
        case syms of
          [] -> queue
          sym:syms' -> let (p, _, _) = (delta m) q sym in
                         addTransitions q syms' (p : queue)
      loop seen queue =
        case queue of
          [] -> return ()
          q:qs -> if not (q `elem` (states m))
                  then error ("Reachable state " ++ show q ++ " not defined in states list")
                  else if not (q `elem` seen) then
                         do putStrLn (" state " ++ show q)
                            loop (q : seen) (addTransitions q (tapeAlpha m) qs)
                       else loop seen qs in
    do putStrLn "----------------------------------------"
       putStrLn "Validating Turing machine:"
       loop [] [start m]
       putStrLn "----------------------------------------"

run :: TM -> String -> IO String
run m input =
  let loop c =
        if isAcceptConfig m c
        then return "ACCEPT"
        else if isRejectConfig m c
        then return "REJECT"
        else do c' <- return (step m c)
                printConfig m c'
                loop c'
      init = startConfig m input in
    do validateStates m
       printConfig m init
       loop init



-- QUESTION 2

dummy :: TM
dummy = TM { states = [0, 1],
             inputAlpha = ['x'],
             tapeAlpha = ['x', '_'],
             start = 0,
             accept = 1,
             reject = 0,
             delta = \q -> \s -> (q, s, -1)}

tm_ab3 :: TM
tm_ab3 = dummy


tm_palindrome :: TM
tm_palindrome = dummy



-- QUESTION 3

compose :: [a -> a] -> (a -> a)
compose fs =
  error "Not implemented"


prefixes :: [a] -> [[a]]
prefixes xs =
  error "Not implemented"


suffixes :: [a] -> [[a]]
suffixes xs =
  error "Not implemented"


maxElement :: [Int] -> Int -> Int
maxElement xs def =
  error "Not implemented"
