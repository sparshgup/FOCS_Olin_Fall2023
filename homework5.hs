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
startConfig m input =
  error "Copy your functions from homework 4!"


isAcceptConfig :: TM -> Config -> Bool
isAcceptConfig m c =
  error "Copy your functions from homework 4!"


isRejectConfig :: TM -> Config -> Bool
isRejectConfig m c =
  error "Copy your functions from homework 4!"


getNth :: [Char] -> Int -> Char
getNth init pos =
  error "Copy your functions from homework 4!"


replaceNth :: [Char] -> Int -> Char -> [Char]
replaceNth init pos sym =
  error "Copy your functions from homework 4!"


step :: TM -> Config -> Config
step m c =
  error "Copy your functions from homework 4!"


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
tm_not = dummyTM


tm_or :: TM
tm_or = dummyTM


tm_increment :: TM
tm_increment = dummyTM


tm_add :: TM
tm_add = dummyTM




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
p_add = dummyRMP


p_sub :: RMP
p_sub = dummyRMP


p_max :: RMP
p_max = dummyRMP


p_diff :: RMP
p_diff = dummyRMP



-- QUESTION 3


p_mult :: RMP
p_mult = dummyRMP


p_double :: RMP
p_double = dummyRMP


p_square :: RMP
p_square = dummyRMP


p_cube :: RMP
p_cube = dummyRMP
