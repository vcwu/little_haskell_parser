little_haskell_parser
=====================

Final Project - CS 441 Programming Languages: Design and Implementation

Your final project is to implement a simple parser in a functional language. Using the Haskell 
language (www.haskell.org), write a parser to assess expressions under the grammar for simple 
arithmetic expressions on p. 184 of your text. 
 
<expr> ==> <term> { ( + | - ) <term> } 
<term> ==> <factor> { (* | / ) <factor> } 
<factor> ==> id | integer constant | ( <expr> ) 
 
 To keep things as simple as possible, you can make the following assumptions: 
 
 The expressions will be in a plain text file called input.txt. You can put the input file into 
the same directory as your program code (the default directory). 
 The text file will contain one expression per line. Each line with an expression will end 
with a newline (‘\n’). This means there is a blank (0-length) line at the end of the file, 
which marks the end of input. 
 All identifiers will be single alphabetic letters. Identifiers are case-sensitive. 
 All numeric values will be integers > 0. 
 All tokens and identifiers will be separated by whitespace. The expression A+(B*c)/2 
would be in the file as: A + ( B * c ) / 2 
 Output is to the screen: “ACCEPT” if the line is a valid expression under the grammar, or 
“REJECT” if it is not. No diagnostic or tracking output is required, but you may find it 
useful while developing your program. 
 
Remember, a parser does not evaluate the semantics of the expression—whether variables are 
defined, whether the expression makes sense in context, etc—only whether the expressions are 
mechanically well-formed according to the specified grammar. Thus, your parser does not have 
to check for divide-by-zero errors, for example. 
