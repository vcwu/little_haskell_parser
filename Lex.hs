--Victoria Wu
--Fall 2013 
--CS 441 Programming Languages
--
--References:
--Hutton "Monadic Parsing in Haskell"
--Scheiber "Constructing a parser combinator in Haskell" from blog.maxscheiber.com


module Lex
(
parse,	--to be honest I'm not sure if these are needed
parseChar,
getNextToken,
helloWorld
) where

--Type Parser
--	fn that eats string
--	returns list [a,String b] where
--		a is the parsed thingy
--		after is the unparsed latter bit
newtype Parser a = Parser (String -> [ (a, String) ]  )

--Passing results from one parser to another.
--	Parse input string with parser p.
--	For each possible solution (a,cs')
--		X
instance Monad Parser where
	return a = Parser (\cs -> [(a,cs)])
	p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs] )



--hi level parser
-----------------------------
parse :: Parser a -> String -> [ (a, String ) ]
parse (Parser par) str = par str

--itty bitty parsers
-----------------------------
parseChar:: Parser Char
parseChar = Parser $ \cs -> case cs of
	(x:xs) 	-> [ (x, xs) ]
	[] 		-> []


--Parser Combinators
------------------------------


--Alternation
--Combines results of two parsers. 

--Concatenation - either parser
--	Parses with first parser. 
--		If it works, returns results.
--		If it doesn't work, parses with second parser.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \cs -> case parse p1 cs of
	[] -> parse p2 cs
	ps -> ps


getNextToken :: [Char] -> [Char]
getNextToken stream = "meep"

helloWorld :: [Char] -> [Char]
helloWorld name = "Hello" ++ name
