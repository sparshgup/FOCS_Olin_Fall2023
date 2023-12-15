{-

--------------------------------------------------

HOMEWORK 5

Due: Sun, Oct 29, 2023 (23h59)

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
{-# HLINT ignore "Use camelCase" #-}

import Control.Monad
import Text.Printf



-- ----------------------------------------------------------
--
-- TURING MACHINES.
--
-- ----------------------------------------------------------


{-
    Type for deterministic Turing machines.
    States are integers.
-}

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


-- Copy your functions from homework 4!

startConfig :: TM -> String -> Config
startConfig m input = Config { state = start m,
                               tape = input,
                               position = 1
                             }


isAcceptConfig :: TM -> Config -> Bool
isAcceptConfig m c = state c == accept m


isRejectConfig :: TM -> Config -> Bool
isRejectConfig m c = state c == reject m


getNth :: [Char] -> Int -> Char
getNth init pos
  | pos < 1 || pos > length init = error "Out of bounds"
  | otherwise = init !! (pos - 1)


replaceNth :: [Char] -> Int -> Char -> [Char]
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


-- Function to "run" a Turing machine on a given string.

run :: TM -> String -> IO String
run m input =
  let printConfig m c =
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
          loop [] [start m]
      loop c =
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


-- QUESTION 1

dummyTM :: TM
dummyTM = TM { states = [0, 1],
               inputAlpha = ['x'],
               tapeAlpha = ['x', '_'],
               start = 0,
               accept = 1,
               reject = 0,
               delta = \q -> \s -> (q, s, -1)}


tm_not :: TM
tm_not = let d state sym =
              case (state, sym) of
                (1, '#') -> (2, '#', 1)
                (2, '1') -> (3, 'X', 1)
                (2, '0') -> (4, 'X', 1)
                (2, 'X') -> (2, 'X', 1)
                (2, '#') -> (2, '#', 1)
                (2, '_') -> (777, '_', 1)
                (3, '0') -> (3, '0', 1)
                (3, '1') -> (3, '1', 1)
                (3, '#') -> (5, '#', 1)
                (3, '_') -> (666, '_', 1)
                (4, '0') -> (4, '0', 1)
                (4, '1') -> (4, '1', 1)
                (4, '#') -> (6, '#', 1)
                (4, '_') -> (666, '_', 1)
                (5, '0') -> (7, 'X', -1)
                (5, '1') -> (666, '1', 1)
                (5, 'X') -> (5, 'X', 1)
                (6, '0') -> (666, '0', 1)
                (6, '1') -> (7, 'X', -1)
                (6, 'X') -> (6, 'X', 1)
                (7, 'X') -> (7, 'X', -1)
                (7, '#') -> (8, '#', -1)
                (8, '0') -> (8, '0', -1)
                (8, '1') -> (8, '1', -1)
                (8, 'X') -> (2, 'X', 1)
                (_, c) -> (666, c, 1) in
         TM { states = [1, 2, 3, 4, 5, 6, 7, 8, 666, 777],
              inputAlpha = ['0', '1', '#'],
              tapeAlpha = ['0', '1', '#', 'X', '_'],
              start = 1,
              accept = 777,
              reject = 666,
              delta = d }


tm_or :: TM
tm_or =  let d state sym =
              case (state, sym) of
                (1, '#') -> (2, '#', 1)
                (2, '0') -> (3, 'X', 1)
                (2, '1') -> (10, 'X', 1)
                (2, 'X') -> (2, 'X', 1)
                (2, '#') -> (2, '#', 1)
                (2, '_') -> (777, '_', 1)
                (3, '0') -> (3, '0', 1)
                (3, '1') -> (3, '1', 1)
                (3, '#') -> (4, '#', 1)
                (4, '0') -> (13, 'X', 1)
                (4, '1') -> (5, 'X', 1)
                (4, 'X') -> (4, 'X', 1)
                (5, '0') -> (5, '0', 1)
                (5, '1') -> (5, '1', 1)
                (5, '#') -> (6, '#', 1)
                (6, '0') -> (666, '0', 1)
                (6, '1') -> (7, 'X', -1)
                (6, 'X') -> (6, 'X', 1)
                (7, '#') -> (8, '#', -1)
                (7, 'X') -> (7, 'X', -1)
                (8, '0') -> (8, '0', -1)
                (8, '1') -> (8, '1', -1)
                (8, 'X') -> (8, 'X', -1)
                (8, '#') -> (9, '#', -1)
                (9, '0') -> (9, '0', -1)
                (9, '1') -> (9, '1', -1)
                (9, 'X') -> (2, 'X', 1)
                (10, '0') -> (10, '0', 1)
                (10, '1') -> (10, '1', 1)
                (10, '#') -> (11, '#', 1)
                (11, '0') -> (12, 'X', 1)
                (11, '1') -> (12, 'X', 1)
                (11, 'X') -> (11, 'X', 1)
                (12, '0') -> (12, '0', 1)
                (12, '1') -> (12, '1', 1)
                (12, '#') -> (6, '#', 1)
                (13, '0') -> (13, '0', 1)
                (13, '1') -> (13, '1', 1)
                (13, '#') -> (14, '#', 1)
                (14, 'X') -> (14, 'X', 1)
                (14, '0') -> (7, 'X', -1)
                (_, c) -> (666, c, 1) in
         TM { states = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 666, 777],
              inputAlpha = ['0', '1', '#'],
              tapeAlpha = ['0', '1', '#', 'X', '_'],
              start = 1,
              accept = 777,
              reject = 666,
              delta = d }


tm_increment :: TM
tm_increment = let d state sym =
                    case (state, sym) of
                      (1, '#') -> (2, '#', 1)
                      (2, '0') -> (2, '0', 1)
                      (2, '1') -> (2, '1', 1)
                      (2, '#') -> (3, '#', -1)
                      (3, '0') -> (4, 'X', 1)
                      (3, '1') -> (8, 'X', 1)
                      (3, 'X') -> (3, 'X', -1)
                      (3, '#') -> (24, '#', 1)
                      (4, 'X') -> (4, 'X', 1)
                      (4, '#') -> (5, '#', 1)
                      (5, '0') -> (5, '0', 1)
                      (5, '1') -> (5, '1', 1)
                      (5, '_') -> (6, '_', -1)
                      (6, '1') -> (7, 'X', -1)
                      (7, '0') -> (7, '0', -1)
                      (7, '1') -> (7, '1', -1)
                      (7, '#') -> (17, '#', -1)
                      (8, 'X') -> (8, 'X', 1)
                      (8, '#') -> (9, '#', 1)
                      (9, '0') -> (9, '0', 1)
                      (9, '1') -> (9, '1', 1)
                      (9, 'X') -> (10, 'X', -1)
                      (9, '_') -> (10, '_', -1)
                      (10, '0') -> (11, 'X', -1)
                      (10, '1') -> (7, 'X', -1)
                      (11, '0') -> (11, '0', -1)
                      (11, '1') -> (11, '1', -1)
                      (11, '#') -> (12, '#', -1)
                      (12, '0') -> (13, 'X', 1)
                      (12, '1') -> (8, 'X', 1)
                      (12, 'X') -> (12, 'X', -1)
                      (13, 'X') -> (13, 'X', 1)
                      (13, '#') -> (14, '#', 1)
                      (14, '0') -> (14, '0', 1)
                      (14, '1') -> (14, '1', 1)
                      (14, 'X') -> (15, 'X', -1)
                      (15, '1') -> (16, 'X', -1)
                      (16, '0') -> (16, '0', -1)
                      (16, '1') -> (16, '1', -1)
                      (16, '#') -> (3, '#', -1)
                      (17, '0') -> (21, 'X', 1)
                      (17, '1') -> (18, 'X', 1)
                      (17, 'X') -> (17, 'X', -1)
                      (17, '#') -> (24, '#', 1)
                      (18, 'X') -> (18, 'X', 1)
                      (18, '#') -> (19, '#', 1)
                      (19, '0') -> (19, '0', 1)
                      (19, '1') -> (19, '1', 1)
                      (19, 'X') -> (20, 'X', -1)
                      (20, '1') -> (7, 'X', -1)
                      (21, 'X') -> (21, 'X', 1)
                      (21, '#') -> (22, '#', 1)
                      (22, '0') -> (22, '0', 1)
                      (22, '1') -> (22, '1', 1)
                      (22, 'X') -> (23, 'X', -1)
                      (23, '0') -> (7, 'X', -1)
                      (24, 'X') -> (24, 'X', 1)
                      (24, '#') -> (25, '#', 1)
                      (25, 'X') -> (25, 'X', 1)
                      (25, '_') -> (777, '_', 1)
                      (_, c) -> (666, c, 1) in
         TM { states = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
                        15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 666, 777],
              inputAlpha = ['0', '1', '#'],
              tapeAlpha = ['0', '1', '#', 'X', '_'],
              start = 1,
              accept = 777,
              reject = 666,
              delta = d }

tm_add :: TM
tm_add = let d state sym =
                    case (state, sym) of
                      (1, '#') -> (2, '#', 1)
                      (2, '0') -> (2, '0', 1)
                      (2, '1') -> (2, '1', 1)
                      (2, '#') -> (3, '#', -1)
                      (3, '0') -> (16, 'X', 1)
                      (3, '1') -> (4, 'X', 1)
                      (3, 'X') -> (3, 'X', -1)
                      (3, '#') -> (777, '#', 1)
                      (4, 'X') -> (4, 'X', 1)
                      (4, '#') -> (5, '#', 1)
                      (5, '0') -> (5, '0', 1)
                      (5, '1') -> (5, '1', 1)
                      (5, '#') -> (6, '#', -1)
                      (5, 'X') -> (6, 'X', -1)
                      (6, '0') -> (7, 'X', 1)
                      (6, '1') -> (11, 'X', 1)
                      (7, 'X') -> (7, 'X', 1)
                      (7, '#') -> (8, '#', 1)
                      (8, '0') -> (8, '0', 1)
                      (8, '1') -> (8, '1', 1)
                      (8, '_') -> (9, '_', -1)
                      (8, 'X') -> (9, 'X', -1)
                      (9, '1') -> (10, 'X', -1)
                      (10, '0') -> (10, '0', -1)
                      (10, '1') -> (10, '1', -1)
                      (10, '#') -> (15, '#', -1)
                      (11, 'X') -> (11, 'X', 1)
                      (11, '#') -> (12, '#', 1)
                      (12, '0') -> (12, '0', 1)
                      (12, '1') -> (12, '1', 1)
                      (12, 'X') -> (13, 'X', -1)
                      (12, '_') -> (13, '_', -1)
                      (13, '0') -> (14, 'X', -1)
                      (14, '0') -> (14, '0', -1)
                      (14, '1') -> (14, '1', -1)
                      (14, '#') -> (23, '#', -1)
                      (15, '0') -> (15, '0', -1)
                      (15, '1') -> (15, '1', -1)
                      (15, 'X') -> (15, 'X', -1)
                      (15, '#') -> (3, '#', -1)
                      (16, 'X') -> (16, 'X', 1)
                      (16, '#') -> (17, '#', 1)
                      (17, '0') -> (17, '0', 1)
                      (17, '1') -> (17, '1', 1)
                      (17, 'X') -> (18, 'X', -1)
                      (17, '#') -> (18, '#', -1)
                      (18, '0') -> (33, 'X', 1)
                      (18, '1') -> (19, 'X', 1)
                      (19, 'X') -> (19, 'X', 1)
                      (19, '#') -> (20, '#', 1)
                      (20, '0') -> (20, '0', 1)
                      (20, '1') -> (20, '1', 1)
                      (20, 'X') -> (21, 'X', -1)
                      (20, '_') -> (21, '_', -1)
                      (21, '1') -> (22, 'X', -1)
                      (22, '0') -> (22, '0', -1)
                      (22, '1') -> (22, '1', -1)
                      (22, '#') -> (15, '#', -1)
                      (23, '0') -> (23, '0', -1)
                      (23, '1') -> (23, '1', -1)
                      (23, 'X') -> (23, 'X', -1)
                      (23, '#') -> (24, '#', -1)
                      (24, '0') -> (25, 'X', 1)
                      (24, '1') -> (37, 'X', 1)
                      (24, 'X') -> (24, 'X', -1)
                      (25, 'X') -> (25, 'X', 1)
                      (25, '#') -> (26, '#', 1)
                      (26, '0') -> (26, '0', 1)
                      (26, '1') -> (26, '1', 1)
                      (26, 'X') -> (27, 'X', -1)
                      (27, '0') -> (28, 'X', 1)
                      (27, '1') -> (44, 'X', 1)
                      (28, 'X') -> (28, 'X', 1)
                      (28, '#') -> (29, '#', 1)
                      (29, '0') -> (29, '0', 1)
                      (29, '1') -> (29, '1', 1)
                      (29, 'X') -> (30, 'X', -1)
                      (30, '1') -> (31, 'X', -1)
                      (31, '0') -> (31, '0', -1)
                      (31, '1') -> (31, '1', -1)
                      (31, '#') -> (32, '#', -1)
                      (32, '0') -> (32, '0', -1)
                      (32, '1') -> (32, '1', -1)
                      (32, 'X') -> (32, 'X', -1)
                      (32, '#') -> (3, '#', -1)
                      (33, 'X') -> (33, 'X', 1)
                      (33, '#') -> (34, '#', 1)
                      (34, '0') -> (34, '0', 1)
                      (34, '1') -> (34, '1', 1)
                      (34, '_') -> (35, '_', -1)
                      (34, 'X') -> (35, 'X', -1)
                      (35, '0') -> (36, 'X', -1)
                      (36, '0') -> (36, '0', -1)
                      (36, '1') -> (36, '1', -1)
                      (36, '#') -> (15, '#', -1)
                      (37, 'X') -> (37, 'X', 1)
                      (37, '#') -> (38, '#', 1)
                      (38, '0') -> (38, '0', 1)
                      (38, '1') -> (38, '1', 1)
                      (38, 'X') -> (39, 'X', -1)
                      (39, '0') -> (40, 'X', 1)
                      (39, '1') -> (47, 'X', 1)
                      (40, 'X') -> (40, 'X', 1)
                      (40, '#') -> (41, '#', 1)
                      (41, '0') -> (41, '0', 1)
                      (41, '1') -> (41, '1', 1)
                      (41, 'X') -> (42, 'X', -1)
                      (42, '0') -> (43, 'X', -1)
                      (43, '0') -> (43, '0', -1)
                      (43, '1') -> (43, '1', -1)
                      (43, '#') -> (23, '#', -1)
                      (44, 'X') -> (44, 'X', 1)
                      (44, '#') -> (45, '#', 1)
                      (45, '0') -> (45, '0', 1)
                      (45, '1') -> (45, '1', 1)
                      (45, 'X') -> (46, 'X', -1)
                      (46, '0') -> (43, 'X', -1)
                      (47, 'X') -> (47, 'X', 1)
                      (47, '#') -> (48, '#', 1)
                      (48, '0') -> (48, '0', 1)
                      (48, '1') -> (48, '1', 1)
                      (48, 'X') -> (49, 'X', -1)
                      (49, '1') -> (43, 'X', -1)
                      (_, c) -> (666, c, 1) in
         TM { states = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
                        15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 
                        26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
                        38, 39, 40, 41, 42, 43, 44, 45, 46, 47 , 48, 49, 666, 777],
              inputAlpha = ['0', '1', '#'],
              tapeAlpha = ['0', '1', '#', 'X', '_'],
              start = 1,
              accept = 777,
              reject = 666,
              delta = d }




-- ----------------------------------------------------------
--
-- REGISTER MACHINES.
--
-- ----------------------------------------------------------

{-
    The type for instructions.
    Each row is a constructor representing an instruction.
    You do not have to understand the details.
-}

data Instruction =
    INC String
  | DEC (String, Int)
  | JUMP Int
  | TRUE
  | FALSE
  | EQUAL (String, String)


showInstr :: Instruction -> String
showInstr ins =
  case ins of
    INC n -> printf "INC %s" n
    DEC (n, addr) -> printf "DEC (%s, %04d)" n addr
    JUMP addr -> printf "JUMP %04d" addr
    TRUE -> "TRUE"
    FALSE -> "FALSE"
    EQUAL (r1, r2) -> printf "EQUAL (%s, %s)" r1 r2


{-
    A register machine program (RMP) is a combination of
    a list of register names and a list of instructions.
-}

data RMP = RMP [String] [Instruction]


{-
    Run a register machine program directly.

    Takes a program and a list of initial values for the first few registers.
    Registers that do not get inital values are initialized to 0.
-}

get lst n =
  case lst of
    [] -> error "Empty list"
    x:xs -> if n == 0 then x else get xs (n - 1)

set lst n v =
  case lst of
    [] -> error "Empty list"
    x:xs -> if n == 0 then v : xs else x : (set xs (n - 1) v)

index lst t =
  case lst of
    [] -> error "Not found"
    x:xs -> if x == t then 0 else 1 + (index xs t)

fill lst len =
  case (lst, len) of
    (_, 0) -> []
    ([], n) -> 0 : fill [] (n - 1)
    (x:xs, n) -> x : fill xs (n - 1)

runRMP :: RMP -> [Int] -> IO Bool
runRMP p nums =
  let RMP rmap parray = p
      registers = fill nums (length rmap)
      loop addr regs =
        do putStr (printf "%04d: %-20s" addr (showInstr (get parray addr)))
           putStrLn (concat (map (printf "%3d ") regs))
           case (get parray addr) of
              INC (r) -> let r' = index rmap r
                             regs' = set regs r' ((get regs r') + 1) in
                           loop (addr + 1) regs'
              DEC (r, addr') -> let r' = index rmap r
                                    v = get regs r' in
                                  if v > 0 then
                                    let regs' = set regs r' (v - 1) in loop (addr + 1) regs'
                                  else
                                    loop addr' regs
              JUMP addr' -> loop addr' regs
              TRUE -> return True
              FALSE -> return False
              EQUAL (r1, r2) -> let r1' = index rmap r1
                                    r2' = index rmap r2 in
                                  return (get regs r1' == get regs r2')  in
  loop 0 registers


-- Some sample programs.

p_reset = RMP ["X"] [
    DEC ("X", 2),
    JUMP 0,
    TRUE
  ]

p_transfer = RMP ["X", "Y"] [
    DEC ("X", 3),
    INC "Y",
    JUMP 0,
    TRUE
  ]

p_succ = RMP ["X", "Z"] [
    INC "X",
    EQUAL ("X", "Z")
  ]




-- QUESTION 2

dummyRMP :: RMP
dummyRMP = RMP [] [FALSE]


p_add :: RMP
p_add = RMP ["X", "Y", "Z"] [
    DEC ("X", 3),
    INC "Y",
    JUMP 0,
    DEC ("Y", 6),
    DEC ("Z", 7),
    JUMP 3,
    DEC ("Z", 8),
    FALSE,
    TRUE
  ]


p_sub :: RMP
p_sub = RMP ["X", "Y", "Z"] [
    DEC ("Y", 3),
    DEC ("X", 3),
    JUMP 0,
    EQUAL ("X", "Z"),
    FALSE,
    TRUE
  ]


p_max :: RMP
p_max = RMP ["X", "Y", "Z"] [
    DEC ("X", 6),
    DEC ("Y", 4),
    DEC ("Z", 6),
    JUMP 0,
    INC "X",
    EQUAL ("X", "Z"),
    EQUAL ("Y", "Z"),
    FALSE,
    TRUE
  ]


p_diff :: RMP
p_diff = RMP ["X", "Y", "Z"] [
    DEC ("X", 5),
    DEC ("Y", 3),
    JUMP 0,
    INC "X",
    EQUAL ("X", "Z"),
    EQUAL ("Y", "Z"),
    FALSE,
    TRUE
  ]


-- QUESTION 3


p_mult :: RMP
p_mult = RMP ["X", "Y", "Z", "P", "T"] [
    DEC ("P", 2),
    JUMP 0,
    DEC ("T", 4),
    JUMP 2,
    DEC ("X", 12),
    DEC ("Y", 9),
    INC "P",
    INC "T",
    JUMP 5,
    DEC ("T", 4),
    INC "Y",
    JUMP 9,
    DEC ("P", 15),
    DEC ("Z", 16),
    JUMP 12,
    DEC ("Z", 17),
    FALSE,
    TRUE
  ]

-- Helper function to transfer p_mult instruction
transfer_p_mult :: String -> String -> String -> Int -> [Instruction]
transfer_p_mult src1 src2 res idx = [
    DEC ("P", idx + 2),
    JUMP idx,
    DEC ("T", idx + 4),
    JUMP (idx + 2),
    DEC (src1, idx + 12),
    DEC (src2, idx + 9),
    INC "P",
    INC "T",
    JUMP (idx + 5),
    DEC ("T", idx + 4),
    INC src2,
    JUMP (idx + 9),
    DEC ("P", idx + 15),
    DEC (res, idx + 16),
    JUMP (idx + 12),
    DEC (res, idx + 17)
  ]

doubleInstruction = [INC "Z", INC "Z"] ++ transfer_p_mult "X" "Z" "Y" 2 ++ [FALSE, TRUE]

p_double :: RMP
p_double = RMP ["X", "Y", "Z", "P", "T"] doubleInstruction


squareInstruction :: [Instruction]
squareInstruction = [DEC ("X", 4), 
                     INC "Z",
                     INC "X0",
                     JUMP 0] 
                     ++ transfer_p_mult "X0" "Z" "Y" 4 
                     ++ [FALSE, TRUE]

p_square :: RMP
p_square = RMP ["X", "Y", "Z", "P", "T", "X0"] squareInstruction


-- Helper function to transfer p_mult for cube instruction
transfer_p_mult_cube :: String -> String -> String -> Int -> [Instruction]
transfer_p_mult_cube src1 src2 res idx = [
    DEC ("P", idx + 2),
    JUMP idx,
    DEC ("T", idx + 4),
    JUMP (idx + 2),
    DEC (src1, idx + 12),
    DEC (src2, idx + 9),
    INC "P",
    INC "T",
    JUMP (idx + 5),
    DEC ("T", idx + 4),
    INC src2,
    JUMP (idx + 9)
  ]

cubeInstruction = [DEC ("X", 5),
                   INC "Z",
                   INC "X0",
                   INC "X1",
                   JUMP 0] 
                   ++ transfer_p_mult_cube "X0" "Z" "Y" 5
                   ++ transfer_p_mult_cube "X1" "Z" "Y" 17
                   ++ 
                   [DEC ("Z", 39), 
                    DEC ("P", 34),
                    DEC ("Y", 38),
                    INC "T",
                    JUMP 30,
                    DEC ("T", 37),
                    INC "P",
                    JUMP 34,
                    JUMP 29,
                    FALSE,
                    EQUAL ("Y", "Z")]


p_cube :: RMP
p_cube = RMP ["X", "Y", "Z", "P", "T", "X0", "X1"] cubeInstruction
