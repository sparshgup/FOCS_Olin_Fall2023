{-

--------------------------------------------------

HOMEWORK 7

Due: Thu, Nov 30, 2023 (23h59)

Name: Sparsh Gupta

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


{-
 - The implementation of parsing and simplication of lambda terms
 -
 - Based on Hutton & Meijer "Monadic Parsing in Haskell"
 -
 -}

import Control.Monad
import Data.Char

newtype Parser a = Parser (String -> [(a,String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser (\cs -> [(a, cs)])
  (<*>) = ap

instance Monad Parser where
  p >>= f = Parser (\cs -> concat [parseCS (f a) cs' | (a,cs') <- parseCS p cs])

parseCS (Parser p) = p

class Monad m => MyMonadPlus m where
  zero :: m a
  plus :: m a -> m a -> m a

instance MyMonadPlus Parser where
  zero = Parser (\cs -> [])
  p `plus` q = Parser (\cs -> parseCS p cs ++ parseCS q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parseCS (p `plus` q) cs of
                           [] -> []
                           (x:xs) -> [x])

item :: Parser Char
item = Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c,cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then pure c else zero}

char :: Char -> Parser Char
char c = sat (c ==)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                   rest a = (do f <- op
                                b <- p
                                rest (f a b))
                            +++ return a

space :: Parser String
space = many (sat isSpace)

apply :: Parser a -> String -> [(a,String)]
apply p = parseCS (do {space; p})

parse :: String -> LTerm
parse s =
  case (apply expr s) of
    [] -> error "Cannot parse string"
    (t, ""):_ -> t
    (t, lft):_ -> error ("Cannot parse at " ++ lft)

data LTerm = LIdent String | LLam String LTerm | LApp LTerm LTerm

instance Show LTerm where
  show = pp

expr :: Parser LTerm
expr = do ts <- many1 term
          return (foldl1 (\r -> \a -> LApp r a) ts)

termIdent :: Parser LTerm
termIdent = do s <- many1 (sat (\c -> isAlpha c || isDigit c || c == '_'))
               space
               return (LIdent s)

termLam :: Parser LTerm
termLam = do char '/'
             s <- many (sat isAlpha)
             space
             char '-'
             char '>'
             space
             t <- expr
             space 
             return (LLam s t)

termParen :: Parser LTerm
termParen = do char '('
               t <- expr
               char ')'
               space
               return t

term :: Parser LTerm
term = termLam +++ termIdent +++ termParen

pp (LIdent x) = x
pp (LLam x t) = "(/" ++ x ++ " -> " ++ pp t ++ ")"
pp (LApp t1 (LApp t3 t4)) = (pp t1) ++ " (" ++ pp (LApp t3 t4) ++ ")"
pp (LApp t1 t2) = (pp t1) ++ " " ++ (pp t2)

rename (LIdent x) old nw = if x == old then LIdent nw else LIdent x
rename (LLam x t) old nw = if x == old then LLam x t else LLam x (rename t old nw)
rename (LApp t1 t2) old nw = LApp (rename t1 old nw) (rename t2 old nw)

fv (LIdent x) = [x]
fv (LLam x t) = [y | y <- fv t, x /= y]
fv (LApp t1 t2) = (fv t1) ++ (fv t2)

-- Note that this is NOT capture avoiding, so don't subsitute open expressions...

substitute (LIdent x) s n = if x == s then n else LIdent x
substitute (LLam x t) s n = if x == s then LLam x t else LLam x (substitute t s n)
substitute (LApp t1 t2) s n = LApp (substitute t1 s n) (substitute t2 s n)

reduce (LIdent s) = Nothing
reduce (LLam s t) = reduce t >>= \t' -> Just (LLam s t')
reduce (LApp (LLam s t1) t2) = Just (substitute t1 s t2)
reduce (LApp t1 t2) = case reduce t1 of
                        Nothing -> reduce t2 >>= \t2' -> Just (LApp t1 t2')
                        Just t1' -> Just (LApp t1' t2)

expand defs t = foldl (\r -> \(n, d) -> substitute r n d) t defs

expandAll :: [(String, LTerm)] -> [(String, LTerm)]
expandAll defs = 
  let loop doneDefs [] = doneDefs
      loop doneDefs ((name, df):dfs) = loop ((name, expand doneDefs df) : doneDefs) dfs in
    loop [] defs

threshold = 5000

simplify :: [(String, String)] -> String -> IO LTerm
simplify rawDefs term =
  let default_defs = [
         ("true", "(/x -> (/y -> x))"),
         ("false", "(/x -> (/y -> y))"),
         ("if", "(/c -> (/x -> (/y -> c x y)))"),
         ("_0","(/f -> (/x -> x))"),
         ("_1","(/f -> (/x -> f x))"),
         ("_2","(/f -> (/x -> f (f x)))"),
         ("_3","(/f -> (/x -> f (f (f x))))"),
         ("_4","(/f -> (/x -> f (f (f (f x)))))"),
         ("_5","(/f -> (/x -> f (f (f (f (f x))))))"),
         ("succ","(/n -> (/f -> (/x -> (n f) (f x))))"),
         ("plus","(/m -> (/n -> (/f -> (/x -> (m f) (n f x)))))"),
         ("times","(/m -> (/n -> (/f -> (/x -> m (n f) x))))"),
         ("iszero","(/n -> n (/x -> false) true)"),
         ("pred","(/n -> (/f -> (/x -> n (/g -> (/h -> h (g f))) (/u -> x) (/u -> u))))"),
         ("pair","(/x -> (/y -> (/s -> s x y)))"),
         ("first", "(/p -> p (/x -> (/y -> x)))"),
         ("second", "(/p -> p (/x -> (/y -> y)))"),
         ("just", "(/a -> (/s -> (/n -> s a)))"),
         ("nothing", "(/s -> (/n -> n))"),
         ("empty", "(/ne -> (/e -> e))"),
         ("cons", "(/x -> (/y -> (/ne -> (/e -> ne x y))))"),
         ("head", "(/lst -> lst (/x -> /y -> x) false)"),
         ("tail", "(/lst -> lst (/x -> /y -> y) false)"),
         ("Theta", "(/x -> (/y -> y ((x x) y))) (/x -> (/y -> y ((x x) y)))"),
         ("sumto","Theta (/f -> (/n -> (iszero n) _0 (plus n (f (pred n)))))")
        ]   
      defs = expandAll [(n, parse d) | (n, d) <- default_defs ++ rawDefs]
      loop n term = do putStr (pp term)
                       putStr "\n"
                       if n > threshold
                         then error ("Failed to find normal form after " ++ show threshold ++ " simplifications")
                       else
                         case (reduce term) of
                           Nothing -> return term
                           Just t -> do putStr " => "
                                        loop (n + 1) t  in
  do putStr "----------------------------------------\n"
     res <- loop 0 (expand defs (parse term))
     putStr "----------------------------------------\n"
     return res




------------------------------------------------------------
-- Put your lambda calculus expressions in this list.
--
-- You can add helper functions if you need, but make sure they appear
-- _before_ any expressions that uses them in the list.
--


qdefs = [
    
    -- QUESTION 1

    ("and", "not_implemented"),
    ("or", "not_implemented"),
    ("not", "not_implemented"),
    ("minus", "not_implemented"),
    ("ge", "not_implemented"),
    ("gt", "not_implemented"),
    ("max", "not_implemented"),
    ("min", "not_implemented"),

    -- QUESTION 2
    
    ("int", "not_implemented"),
    ("neg_int", "not_implemented"),
    ("abs_int", "not_implemented"),
    ("plus_int", "not_implemented"),

    -- QUESTION 3    

    ("default", "not_implemented"),
    ("either", "not_implemented"),
    ("sum", "not_implemented"),
    ("nth", "not_implemented")
  ]
