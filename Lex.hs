module Lex
(
getNextToken,
helloWorld
) where

getNextToken :: [Char] -> [Char]
getNextToken stream = "meep"

helloWorld :: [Char] -> [Char]
helloWorld name = "Hello" ++ name
