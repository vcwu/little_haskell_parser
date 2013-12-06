--Victoria Wu
--Fall 2013 
--CS 441 Programming Languages
--
--Core Monadic Parsing Library
--
--The basic functions are all taken and in some cases modified from the following resources. 
--References:
--Hutton "Monadic Parsing in Haskell"
--Scheiber "Constructing a parser combinator in Haskell" from blog.maxscheiber.com
-- ****************************************

module Lex
(
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

--class Monad m => MonadZero m where
--	zero :: m a
--instance MonadZero Parser where
--	zero = Parser (\cs -> [])

--Union operator
--class MonadZero m => MonadPlus m where
--	(++) :: m a -> m a -> m a
--instance MonadPlus Parser where
--	p ++ q = Parser (\cs -> parse p cs ++ parse q cs)

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

--many
--applies parser p zero or more times
--returns  not all possibilities, but only the largest
--liftM :: (Monad m) => (a -> b) -> m a -> m b 
--liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c *same as liftM, but with 2 params
--many :: Parser a -> Parser [a]
--many p = ( liftM2 (:) p (many p) ) <|>  return []
--many p = ( liftM2 (:) p (many p) )  `choose`  return []

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

--token:: Parser a -> Parser a
--token p = do {a <- p; many (satisfy isSpace) ; return a }

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


--	<factor> -> id | int_constant | { <expr> }
factor :: Parser String
factor = ident <|> number 

--tester try to parse MULTIPLE THINGIES - the dreaded { thing } in EBNF 
--tester:: Parser String -> Parser String
--tester par = Parser $ \cs -> case parse par (cs) of
--	[] -> []
--	[(a, "")] -> [(a,"")]
--	ps -> tester par 

--WHat I want this to do.
--Repeat this parser ONE or more times :(
--HOLY COW IT WORKS
magic :: Parser a -> Parser a
magic par = Parser $ \cs -> case parse par cs of
	[] -> []
	[(a,"")] -> [(a,"")]
	[(a,cs')] -> parse (magic par) cs' 

-- now to fix my bug...
-- it needs to repeat the parser ZERO or more times
--magicZero :: Parser a -> Parser a
--magicZero par = 


--BUG TIME GUYS!!
--lexical error in string/character literal at character 'a'
--parse termlette "/ asdfasdf"
-- ..  i don't even know anymore. i think it's cause it's an escape character T.T


-- Made specifically to account for the { blah blah }, of needing zero or more.
--repeatParser :: Parser String  -> Parser String
--repeatParser par = Parser $ \ (a,cs) -> ("testing", cs)
--repeatParser p = ( liftM2 (:) p (repeatParser p) ) <|>  return []

-- parser for this thingy -> { (*|/) <factor> }
termlette :: Parser String
termlette = token multOp >> token factor

--buggggg -> so this should return TRUE is there isn't a termlette
--	<term> -> <factor> { (*|/) <factor> }
term :: Parser String
term = token factor >> ( magic termlette)

-- parser for this thingy -> {(+|-) <term>  } 
exprlette:: Parser String
exprlette = token addOp >> token term 

--	<expr> -> <term> {(+|-) <term> }
expr :: Parser String
expr = token term >>  (magic exprlette)

--ok problem... when I chain many1, things get WEIIRD. 
--but i dont know how to chain anything then :( this will only check the first toke n bleeehhhhh
accept :: String -> String
accept str = case parse (factor) str of
	[] -> "Reject"
	ps -> "Accept" 
