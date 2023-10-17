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
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use camelCase" #-}


import Control.Monad
import Distribution.Simple.Utils (xargs)
import Data.Text (replace)

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
startConfig m input = Config { state = start m,
                               tape = input,
                               position = 1
                             }


isAcceptConfig :: TM -> Config -> Bool
isAcceptConfig m c = state c == accept m


isRejectConfig :: TM -> Config -> Bool
isRejectConfig m c = state c == reject m


getNth :: [a] -> Int -> a
getNth init pos
  | pos < 1 || pos > length init = error "Out of bounds"
  | otherwise = init !! (pos - 1)


replaceNth :: [a] -> Int -> a -> [a]
replaceNth init pos sym
  | pos < 1 || pos > length init = error "Out of bounds"
  | otherwise = take (pos - 1) init ++ [sym] ++ drop pos init


step :: TM -> Config -> Config
step m c =
  let currentState = state c
      currentPosition = position c
      currentTape = tape c

      readSymbol = if currentPosition <= length currentTape 
        then tape c !! (currentPosition - 1) 
        else '_'

      (newState, writeSymbol, moveDirection) = delta m currentState readSymbol

      newTape = if currentPosition <= length currentTape
        then replaceNth currentTape currentPosition writeSymbol
        else tape c ++ replicate (currentPosition - length currentTape - 1) '_' ++ [writeSymbol]

      newPosition = if moveDirection == 1 then currentPosition + 1 else currentPosition - 1

  in Config { state = newState,
              tape = newTape,
              position = newPosition }



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
tm_ab3 = let d sym state =
              case (sym, state) of
                (1, 'a') -> (2, 'X', 1)
                (1, '_') -> (777, '_', 1)
                (2, 'a') -> (2, 'a', 1)
                (2, 'Y') -> (2, 'Y', 1)
                (2, 'b') -> (3, 'Y', 1)
                (3, 'b') -> (4, 'Y', 1)
                (4, 'b') -> (4, 'Y', -1)
                (4, 'Y') -> (4, 'Y', -1)
                (4, 'a') -> (5, 'a', -1)
                (4, 'X') -> (6, 'X', 1)
                (5, 'X') -> (1, 'X', 1)
                (5, 'a') -> (5, 'a', -1)
                (6, 'Y') -> (6, 'Y', 1)
                (6, '_') -> (777, '_', 1)
                (_, c) -> (666, c, 1) in
         TM { states = [1, 2, 3, 4, 5, 6, 777, 666],
              inputAlpha = ['a', 'b'],
              tapeAlpha = ['a', 'b', 'X', 'Y', '_'],
              start = 1,
              accept = 777,
              reject = 666,
              delta = d }


tm_palindrome :: TM
tm_palindrome = let d sym state =
                      case (sym, state) of
                        (1, 'a') -> (2, 'X', 1)
                        (1, 'b') -> (3, 'Y', 1)
                        (1, 'X') -> (777, 'X', 1)
                        (1, 'Y') -> (777, 'Y', 1)
                        (1, '_') -> (777, '_', 1)
                        (2, 'a') -> (2, 'a', 1)
                        (2, 'b') -> (2, 'b', 1)
                        (2, 'X') -> (4, 'X', -1)
                        (2, 'Y') -> (4, 'Y', -1)
                        (2, '_') -> (4, '_', -1)
                        (3, 'a') -> (3, 'a', 1)
                        (3, 'b') -> (3, 'b', 1)
                        (3, 'X') -> (5, 'X', -1)
                        (3, 'Y') -> (5, 'Y', -1)
                        (3, '_') -> (5, '_', -1)
                        (4, 'a') -> (6, 'X', -1)
                        (4, 'X') -> (777, 'X', 1)
                        (4, 'Y') -> (777, 'Y', 1)
                        (5, 'b') -> (6, 'Y', -1)
                        (5, 'X') -> (777, 'X', 1)
                        (5, 'Y') -> (777, 'Y', 1)
                        (6, 'a') -> (6, 'a', -1)
                        (6, 'b') -> (6, 'b', -1)
                        (6, 'X') -> (1, 'X', 1)
                        (6, 'Y') -> (1, 'Y', 1)
                        (_, c) -> (666, c, 1) in
                TM { states = [1, 2, 3, 4, 5, 6, 666, 777],
                      inputAlpha = ['a', 'b'],
                      tapeAlpha = ['a', 'b', '_'],
                      start = 1,
                      accept = 777,
                      reject = 666,
                      delta = d }


-- QUESTION 3

compose :: [a -> a] -> (a -> a)
compose fs = foldr (.) id fs


prefixes :: [a] -> [[a]]
prefixes xs = foldr (\x xss -> [] : map (x :) xss) [[]] xs


suffixes :: [a] -> [[a]]
suffixes xs = foldr (\x xss -> xs : xss) [[]] xs


maxElement :: [Int] -> Int -> Int
maxElement xs def =
  case (xs, def) of
    ([], def) -> def
    _         -> foldr max (head xs) xs
