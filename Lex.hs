--Victoria Wu
--Fall 2013 
--CS 441 Programming Languages
--
--Most of the basic functions are taken from tutorial/reference materials mentioned below. 
--The bulk of my work is under "Grammar Time".


--KNOWN BUGS 
--Trying to parse "/" an escape char will not go well 

--References:
--Constructing a parser combinator in haskell - Scheiber
--http://blog.maxscheiber.com/2013/01/constructing-a-parser-combinator-in-haskell-part-1/
--
--Monadic Parsing Lecture Notes
--http://www.seas.upenn.edu/~cis552/12fa/lectures/Parsers.html
--
--Monadic Parser Combinators - Hutton, Meijer
--http://www.cs.nott.ac.uk/~gmh/monparsing.pdf
--
--Functional Pearls: Monadic Parsing in Haskell - Hutton, Meijer
--http://www.cs.nott.ac.uk/~gmh/pearl.pdf
--
--Combinator Parsing : A Short Tutorial - Swierstra
--http://www.cs.tufts.edu/~nr/cs257/archive/doaitse-swierstra/combinator-parsing-tutorial.pdf
-- ****************************************

module Lex
(
accept
) where
import Data.String
import Data.Char
import Control.Monad
--Type Parser
--	fn that eats string
--	returns list [a,String b] where
--		a is the parsed thingy
--		after is the unparsed latter bit
newtype Parser a = Parser (String -> [ (a, String) ]  ) 

newtype MyStr a = String a

--Passing results from one parser to another.
--	Parse input string with parser p.
--	For each possible solution (a,cs')
--		f a is a parser applied to string cs' 
--	bind :T is Parser a -> (a -> Parser b) -> Parser b
instance Monad Parser where
	return a = Parser (\cs -> [(a,cs)])
	p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs] )

instance MonadPlus Parser where
	mzero =  Parser (\cs -> [])
	p `mplus` q  = Parser (\cs -> parse p cs ++ parse q cs) 

-- ****************************************

--the action, to parse
-----------------------------
parse :: Parser a -> String -> [ (a, String ) ]
parse (Parser par) = par 

--itty bitty parsers (primitives)
-----------------------------
item:: Parser Char						--get first char.
item = Parser $ \cs -> case cs of
	(x:xs) 	-> [ (x, xs) ]
	[] 		-> []

zero :: Parser a						--always fail.
zero = Parser (\inp -> [])

result :: a -> Parser a					--always succeeds without eating input
result v = Parser $ \inp -> [(v, inp)] 

--Parser Operators
------------------------------

--Deterministic choice operator.
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \cs -> case parse ( p `mplus`  q ) cs of
	[] -> []
	(x:xs) -> [x]
--	ps -> ps

--Execute first parser. If it fails, execute second one -> only one right answer
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \cs -> case parse p cs of
	[] -> parse q cs
	ps -> ps

--Parser Combinators
------------------------------

--Satisfy
--Given a predicate, yield parser that eats a char if true, fail if false
-- 		so apparently do blocks + <- are syntatic sugar for these thingies <<=
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
	c <- item 
	if p c 
		then return c
	else
		zero

--char equality
char :: Char -> Parser Char
char c = satisfy (==c)

alpha, digit, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
space = satisfy isSpace

--string equality
--mapM :: Monad m => (a -> mb) -> [a] -> m [b]
string :: String -> Parser String
string = mapM char --so, mapM is just sequence (map f as) 


--sigh.. the mutualy recursive ones :(
--many - zero or more times
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

--many1 - one or more times
many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}




--Bigger Parsers
---------------------------------

--get an identifier- made up of any alpha char 
ident :: Parser String
ident = many1 alpha

--get a number (for our purposes, not bothering to convert to int)
number :: Parser String
number = many1 digit


-- get rid of those pesky trailing spaces
token :: Parser a -> Parser a
token p = do {a <- p; many space; return a}


-- Grammar time!
-- *****************************************

--addOp
addOp :: Parser String
addOp = string "+" <|> string "-"

--multOp
multOp :: Parser String
multOp = string "*" <|> string "/"


--Parse for the parentheses...
braces :: Parser String
braces = token (string "(") >> expr >> token (string ")")

--	<factor> -> id | int_constant | { <expr> }
factor :: Parser String
factor = ident <|> number <|> braces 

--WHat I want this to do.
--Repeat this parser ONE or more times :(
--HOLY COW IT WORKS
--WHAT IS THIS SORCERY IT WORKS O.O (update #2 with expr YEAAAAAH)
magic :: String -> Parser String ->  Parser String
magic str par = Parser $ \cs -> case parse par cs of
		[(a,"")] -> [(a,"")]
		[] -> [("",cs)]
		[(a,cs')] -> parse (magic a par) cs' 


-- parser for this thingy -> { (*|/) <factor> }
termlette :: Parser String
termlette = token multOp >> token factor 

--	<term> -> <factor> { (*|/) <factor> }
term :: Parser String
term = Parser $ \cs -> case parse (token factor) cs of
	[(a,"")] -> [(a,"")]
	[(a,cs')] -> parse (magic a termlette) cs'
	[] -> []
-- so the cs being passed is the a from the resulting  [(a,cs')] 


-- parser for this thingy -> {(+|-) <term>  } 
exprlette:: Parser String
exprlette = token addOp >> token term 


--	<expr> -> <term> {(+|-) <term> }
expr :: Parser String
expr = Parser $ \cs -> case parse (token term) cs of
	[(a,"")] -> [(a,"")]
	[(a,cs')] -> parse (magic a exprlette) cs'
	[] -> []

-- Top level does this work or not
accept:: String -> String
accept str = case parse (expr) str of
	[(a,"")]-> "ACCEPT"  
	_ -> "REJECT"		--If the resulting string is not COMPLETELY parsed (ie dangling ')'), it's wrong.
