--little baby steps
--let's try and read a file
--now let's do stuff one line at a time




--take while
--do while? - for paren getting

--make a lexar. so given an input string... 
-- break it up into TOKEN (type) and lexeme (the actual thingy)
--	<expr> -> <term> {(+|-) <term> }
--	<term> -> <factor> { (*|/) <factor> }
--	<factor> -> id | int_constant | { <expr> }
import System.Environment
import Lexer
main = do 
	contents <- readFile "input.txt"
	putStr (unlines (parseLines (lines contents)))
--	putStr contents

parseLines [] = []
parseLines (x:xs) = 
	parseOneLine(x) : parseLines(xs)


parseOneLine "" = ""
parseOneLine xs = xs ++ "has been parsed"
