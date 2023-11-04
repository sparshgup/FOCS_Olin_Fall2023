{-

--------------------------------------------------

HOMEWORK 6

Due: Sun, Nov 12, 2023 (23h59)

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


import qualified Data.Set as Set
import Text.Printf
import System.IO.Unsafe


-- QUESTION 1

type Partial b = String -> Maybe b

null_partial :: Partial b
null_partial =
  error "Not implemented"


extend_partial :: String -> b -> (Partial b) -> Partial b
extend_partial arg value p =
  error "Not implemented"


join_partial :: (Partial b) -> (Partial b) -> Partial b
join_partial p1 p2 =
  error "Not implemented"


default_partial :: b -> (Partial b) -> (String -> b)
default_partial b p =
  error "Not implemented"


fail_partial :: (Partial b) -> (String -> b)
fail_partial p =
  error "Not implemented"


dict :: [(String, b)] -> (String -> b)
dict pairs =
  error "Not implemented"



-- QUESTION 2

{-
    Type for grammars.
-}

data CFG = CFG {
  cfg_nterms :: [Char],
  cfg_terms  :: [Char],
  cfg_rules :: [(Char, String)],
  cfg_start :: Char
}

data Grammar = Grammar {
  nterms :: [Char],
  terms :: [Char],
  rules :: [(String, String)],
  start :: Char
}

convert_cfg g =
  Grammar {nterms = cfg_nterms g,
           terms = cfg_terms g,
           rules = [([a], b) | (a, b) <- cfg_rules g],
           start = cfg_start g}


-- Some sample context-free grammars.

anbn = CFG {
  cfg_nterms = ['S'],
  cfg_terms = ['a', 'b'],
  cfg_rules = [('S', ""),
               ('S', "aSb") ],
  cfg_start = 'S'
}

anbm = CFG {
  cfg_nterms = ['S', 'T', 'U'],
  cfg_terms = ['a', 'b'],
  cfg_rules = [('S', "TU"),
               ('T', ""),
               ('T', "aTb"),
               ('U', ""),
               ('U', "Ub")],
  cfg_start = 'S'
}


-- Perform a bounded depth-first search of the rewrite tree.

dfs_path :: [String] -> Int -> Int -> Grammar -> String -> IO [String]
dfs_path init maxdepth maxwidth grammar target =
  let -- Check if lhs is a prefix of str.
      prefix lhs str =
        lhs == take (length lhs) str
      -- Replace prefix lhs of str with rhs.
      replace lhs str rhs =
        rhs ++ drop (length lhs) str
      -- Try to apply rule (lhs, rhs) to str (assuming prefix prf).
      apply_rule prf (lhs,rhs) str =
        if length str < length lhs
          then []
        else if prefix lhs str
          then [prf ++ (replace lhs str rhs)]
        else []
      -- Try to apply every rule in rs to str.
      apply_rules :: [(String, String)] -> String -> [String]
      apply_rules rs str =
        let loop prefix str =
              if str == ""
                then []
              else let rest = loop (prefix ++ take 1 str) (drop 1 str) in
                   (foldl (\res -> \r -> res ++ (apply_rule prefix r str)) [] rs) ++ rest  in
        loop "" str
      -- Check if a string can be obtained from another string by applying a rule in rs.
      obtainable rs start str1 str2 =
        if str1 == ""
          then str2 == start
        else
          let strs = apply_rules rs str1 in
          str2 `elem` strs
      initPart init previous =
        case init of
          [] -> previous
          x:xs -> if obtainable (rules grammar) [start grammar] previous x
                     then initPart xs x
                   else error ("Unacceptable string '" ++ x ++ "' in init derivation")
      actualStart = initPart (case init of
                                [] -> [[start grammar]]
                                _ -> init) ""
      loop q seen =
        do -- putStrLn (show q)
           case q of
             [] -> return []
             ((path,d):q) ->
                  (case path of
                     str:_ -> if length str > maxwidth + length actualStart
                                then loop q seen
                              else if length str == length target && str == target
                                then return path
                              else if Set.member str seen
                                then loop q seen
                              else if d > maxdepth
                                then loop q (Set.insert str seen)
                              else -- let _ = (print_string str; print_newline()) in *)
                                let new_strs = apply_rules (rules grammar) str
                                    new_strs_d = map (\x -> (x:path, d+1)) new_strs
                                    q' = (new_strs_d) ++ q in
                                loop q' (Set.insert str seen)
                     _ -> error "problem - empty path in dfs_path") in
  loop [([actualStart],0)] Set.empty


-- Perform an iteratively deepening depth-first search of the rewrite tree.

idfs_path :: [String] -> Int -> Grammar -> String -> IO [String]
idfs_path init maxdepth grammar target =
  let loop :: Int -> IO [String]
      loop n = do printf "Searching (depth %02d, max width %02d)\n" n n
                  if n > maxdepth
                    then return []
                  else do path <- dfs_path init n n grammar target
                          case path of
                            [] -> loop (n + 1)
                            path -> return path  in
  loop 1


-- Try to generate a string for a given grammar.

generate :: Grammar -> String -> Int -> [String] -> IO Bool
generate grammar str md init =
  let print pre str = do putStr pre
                         putStrLn str
      rev_print path =
        case path of
          [] -> return ()
          [s] -> print "   " s
          s:ss -> do rev_print ss
                     print "-> " s in
  do path <- idfs_path init md grammar str
     rev_print path
     return ((length path) > 0)


generate_cfg :: CFG -> String -> Int -> [String] -> IO Bool
generate_cfg g str md init =
  let grammar = convert_cfg g
      print pre str = do putStr pre
                         putStrLn str
      rev_print path =
        case path of
          [] -> return ()
          [s] -> print "   " s
          s:ss -> do rev_print ss
                     print "-> " s in
  do path <- idfs_path init md grammar str
     rev_print path
     return ((length path) > 0)


-- QUESTION 2

dummy_cfg = CFG {
  cfg_nterms = [],
  cfg_terms = [],
  cfg_rules = [],
  cfg_start = 'S'
}


q2_part_a :: CFG
q2_part_a = dummy_cfg


q2_part_b :: CFG
q2_part_b = dummy_cfg


q2_part_c :: CFG
q2_part_c = dummy_cfg


q2_part_d :: CFG
q2_part_d = dummy_cfg


q2_part_e :: CFG
q2_part_e = dummy_cfg


-- QUESTION 3

{-
   Here's a grammar that is _not_ context-free.
   It's also harder to generate its strings.
-}

anbncn = Grammar {
  nterms = ['S', 'A', 'B', 'C', 'X'],
  terms = ['a', 'b', 'c'],
  rules = [ ("S", ""),
            ("S", "AB"),
            ("B", "XbBc"),
            ("B", ""),
            ("A", "AA"),
            ("AX", "a"),
            ("aX", "Xa"),
            ("bX", "Xb") ],
  start = 'S'
}

{-
Sample derivation:
S
AB
AAB
AAAB
AAAXbBc
AAAXbXbBcc
AAAXbXbXbBccc
AAAXbXbXbccc
AAAXbXXbbccc
AAAXXbXbbccc
AAAXXXbbbccc
AAaXXbbbccc
AAXaXbbbccc
AaaXbbbccc
AaXabbbccc
AXaabbbccc
aaabbbccc
-}


dummy_grammar = Grammar {
  nterms = [],
  terms = [],
  rules = [],
  start = 'S'
}


q3_part_a :: Grammar
q3_part_a = dummy_grammar


-- Please put your sequence of rewrites for the string aabbccdd here

rewrites_part_a_2 = []

-- Please put your sequence of rewrites for the string aaabbbcccddd here

rewrites_part_a_3 = []


q3_part_b :: Grammar
q3_part_b = dummy_grammar


-- Please put your sequence of rewrites for the string abaaabaa here

rewrites_part_b_abaa = []

-- Please put your sequence of rewrites for the string aabbbaabbb here

rewrites_part_b_aabbb = []
