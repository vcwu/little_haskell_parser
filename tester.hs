--little baby steps
--let's try and read a file
--now let's do stuff one line at a time


--take while
--do while? - for paren getting
import System.Environment

main = do 
	contents <- readFile "input.txt"
	putStr (unlines (parseLines (lines contents)))
--	putStr contents

parseLines [] = []
parseLines (x:xs) = 
	parseOneLine(x) : parseLines(xs)


parseOneLine "" = ""
parseOneLine xs = xs ++ "has been parsed"
