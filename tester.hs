
--Victoria Wu
--Fall 2013
--CS 441 Programming Languages
--
--Tester file to take in input file and apply the parser that parses this grammar
--	<expr> -> <term> {(+|-) <term> }
--	<term> -> <factor> { (*|/) <factor> }
--	<factor> -> id | int_constant | { <expr> }
import System.Environment
import Lex (accept)
main = do 
	contents <- readFile "input.txt"
	putStr (unlines (parseLines (lines contents)))
--	putStr contents
--	putStr "done"
	
parseLines [] = []
parseLines (x:xs) = 
	parseOneLine(x) : parseLines(xs)


parseOneLine "" = ""
--parseOneLine xs = xs ++ " => " ++ accept xs
parseOneLine xs = accept xs
