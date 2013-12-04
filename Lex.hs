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
Parser,
parse,	--to execute a Parser	
item,	--Parser to consume one char
many,	--
alpha,
digit,
space,
ident
) where

import Data.Char
import Control.Monad
--Type Parser
--	fn that eats string
--	returns list [a,String b] where
--		a is the parsed thingy
--		after is the unparsed latter bit
newtype Parser a = Parser (String -> [ (a, String) ]  ) 


--Passing results from one parser to another.
--	Parse input string with parser p.
--	For each possible solution (a,cs')
--		f a is a parser applied to string cs' 
--	bind :T is Parser a -> (a -> Parser b) -> Parser b
instance Monad Parser where
	return a = Parser (\cs -> [(a,cs)])
	p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs] )


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

--Union
--Combines results of two parsers. 
choose :: Parser a -> Parser a -> Parser a
p `choose` q = Parser $  \inp -> (parse p inp ++ parse q inp)

--bugggggy :( - > how do I tell if a parser failed??
--deteriministic parser 
--		If first works, returns first result.
--		If it doesn't work, parses with second parser and gets first result.
--		Guaranteed to only return at most one thing.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \cs -> case parse p1 cs of
	[] -> take 1 (parse p2 cs)
	((a,cs'):_) -> [(a,cs')]
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
many :: Parser a -> Parser [a]
many p = ( liftM2 (:) p (many p) ) <|>  return []
--many p = ( liftM2 (:) p (many p) )  `choose`  return []



--Bigger Parsers
---------------------------------

--get an identifier- made up of any alpha char 
ident :: Parser String
ident = many alpha

--get a number (for our purposes, not bothering to convert to int)
number :: Parser String
number = many digit

--	<factor> -> id | int_constant | { <expr> }
factor :: Parser String
factor = ident <|>  number 
