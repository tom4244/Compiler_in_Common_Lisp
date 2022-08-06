This compiler generates assembly code from source code written in a small programming language called "Rat10". Perhaps it can be used as a starting point for writing compilers for other languages in Lisp.

Sample source files of the "Rat10" language are included: sample.rat, sample2.rat, t1.rat, t2.rat, and t3.rat. Sample output files are included for compilation of the "sample.rat" source, with additional examples of output seen when using the "debug", "short", and "long" options explained below. 

How to Use the Compiler

The compiler ("a3.lisp") is written in Common Lisp. To run it, use a Linux system with a version of Common Lisp installed such as SBCL or CMUCL and put the "a3.lisp" file and any source files to be parsed and enter the following:

`lisp -quiet -eval '(load "a3.lisp")'`

With this command as shown, the program will display and save to the output file the Assembly Code Listing, Symbol Table, and any error messages, such as if an identifier is used without having been declared, or if one is declared a second time.

Optionally, more information can be displayed and saved in the output file by entering one of the following words after "lisp" in the command line:

`long`:  Prints out the long version of productions, for example:<br>  
`<Expression>  ::= <Expression> + <Term>  | <Expression>  - <Term>  | <Term>`.

`short`: Prints out the abbreviated version of productions, for example:<br>  
`E  ->  T  EP`.

`debug`: More verbose output to help in checking/debugging including the short version of productions, periodic stack contents, items being pushed to and popped from the `op-stack`, and lines from the assembly table as these things occur.

`sample`: The program will use "sample.rat" as the source file if it exists. It will still prompt for an output file name, which can default to "sample.rat.out" by pressing "Enter" as usual. Note this can also be done by simply entering "sample.rat" when prompted for the source file.

For example, to see the listing with extra debug content enter:

`lisp debug -quiet -eval '(load "a3.lisp")'`

The compiler will prompt for the source file to be parsed, and prompt again for the output file.
Hitting only "enter" for the output filename will default to the same name as the source file with ".out" appended to the end.

Design and Choice of Algorithms

This compiler uses a table-driven top-down parser.

First, the "lexer" function tokenizes the source code text. The lexer works as follows:

Characters from the source file string are matched against a list of characters used in valid lexeme types (`*charlist*`). The program starts in State 1. The next state depends on what the next character is that is read. The states are numbered, and the program uses the state numbers to keep track of which actions to take (`do-state`) to find the tokens in the source file of characters. The states are organized in a table (`*lextable*`). When it is established that a complete lexeme has been received, it is printed with its type, and the program goes back to State 1, ready to process the next character.

Single-character lexemes can be immediately printed with their type. Then the program returns to State 1 and reads the next character.

Some characters could be either a single-character lexeme or part of a two-character lexeme. For such characters, the upcoming character is peeked at (`may-be-token`). If it is seen that this character will not be part of a two-character lexeme, the character is accepted as a single-character lexeme and printed with its type, and the program can then go back to State 1.

If it is seen that a character is the first of what will be a two-character lexeme, processing it as such will be handled by the appropriate state for the second character. The program will arrive at that state using the lextable lookup from the first character's state. The two-character lexeme will be accepted, printed, and the program will go back to State 1.

When a letter is read, the program is in State 2. This means that one or more keywords and/or identifiers are being read. When a character other than a letter, digit, or underscore is received, State 3 is entered. This means that the keyword/identifier string of contiguous characters has ended, and must be searched for keywords and identifiers.

An algorithm (`check-for-keyword`) tries to match the characters, starting at the first character of the string, with all valid keywords. The keywords are tried from longest to shortest. This keeps the software from, for example, reading the valid keyword "endif" as identifier "end" + keyword "if". If no keywords match from the first character in the string, the matching is tried again from the second character, and so on through the string. Thus finding the beginning of a keyword establishes the end of a preceding identifier. To save time, before trying to match a keyword, it is verified that there is even enough room in the string for the current keyword to fit.

When a keyword is found at the beginning of the string, it is accepted. The algorithm will start again at State 1 at the next character after the keyword. If a keyword is found later in the string, the characters before it make a valid identifier, so the identifier is accepted and printed. The program will then backspace enough to process the next character (which will be the first of the previously found keyword), from State 1 as usual. By this method, the algorithm will extract all valid identifiers and keywords from a contiguous string of alphanumerics and underscores of any length.

All identifiers are checked for ending underscores (`handle-ending-underscore`), which if found are printed as an illegal character. This could happen, for example, if a valid keyword appeared immediately after an underscore. Any underscore occuring from State 1, i.e., not coming in as part of an emerging identifier, will cause the lextable lookup to go to State 32, the "illegal character state", and be printed as an illegal character.

When a digit is read, the program is in State 4. This means that a real number or integer is emerging. Characters continue being added to the number until any character that is not a digit or decimal point is read, or until a second decimal point is read. If a second decimal point is read, the number is considered a complete real number with its last digit being the one just before the second decimal point.

With the number complete, the program moves to State 5, and the number is printed as a real, or (if there was no decimal point) as an integer. In the case of a second/ending decimal point, the program will backspace to re-read it in the upcoming State 1, and will see it as an illegal character since it is not between the digits of a real number. Any digits after it will be processed as above, starting a new real or integer. With this method, the program tries to extract as many valid reals and integers as possible from any contiguous string of digits and decimal points.

All characters that are not one of the valid types are printed as belonging to the "illegal" type.

In "Rat10", program structure is as follows: "$$" marks the beginning of a program, which contains an optional function definition section, then another "$$", an optional declaration list, then the statement list of the source code, and finally a third "$$" marking the end.

Terminal symbols are the elementary symbols of a language. Nonterminal symbols (also called "syntactic variables") are replaced by groups of terminal symbols according to the production rules. 

As lexemes are found by `lexer`, the parser holds nonterminals and terminals on a stack, beginning with only the starting symbol and end-of-stack symbol. The parser uses a table to decide what right hand side symbols to replace the nonterminal on the top of stack (TOS) with for given input lexemes. The parser table has one row for each nonterminal symbol in the grammar. There is one column for each terminal or kind of terminal including keywords.

To keep the table to a manageable size for inspection and debugging, it was split into two main parts, one for keywords and identifier lexemes (`*identifier-table*`) and another table for all other lexemes (`*parser-table*`). Where the contents of a cell in the table are long, they are coded by an integer and kept in a separate list (`*rhs-list*`), and the integer code is put in the table cell.

In more detail, when a symbol is received from `lexer`, first the program checks to see if the TOS is a terminal symbol. If it is, the input symbol is checked to see if it matches exactly. If so, the stack is popped and "lexer" is called again for the next input. No productions are printed out yet.

Similarly, if the top of stack contains "ID", "RL", or "IN", the codes for identifier, real, or integer respectively, and `lexer` reports that this is what was sent, the stack is popped and `lexer` is called. It is not necessary for productions to be printed yet at this point.

If the terminal type on TOS does not match the type indicated by `lexer`, or match it exactly, there is a syntax error, so `show-syntax-error` is called.

If TOS was a nonterminal instead of terminal, the table will be used to find the right-hand-side (RHS) that the TOS symbol will be replaced with on the stack. First, the token type of the incoming symbol from `lexer` is checked (as reported by `lexer`) to see which of the two tables (`*identifier-table*` or `*parser-table*`) to use. Then the new RHS is looked up. If there is nothing in the table cell, this indicates no RHS for the given input and TOS, which is a syntax error, so `show-syntax-error` is called and the program exits.

`show-syntax-error` reports slightly different things depending on whether the TOS is a terminal or nonterminal. In the former case, the error is detected when the lexeme received is not what was expected from what is on TOS, so the input lexeme and nonmatching TOS terminal are printed. In the latter case, the error is detected when the table cell is empty, meaning there is no production with a RHS having the TOS NT as a left-hand-side. So then, the input lexeme and a list of all lexemes that could result from the TOS NT are printed. In the table, these lexemes are simply the ones with entries (RHSs or epsilons) in that NT's row. Also, for either of the above two kinds of errors, the stack contents and line number where the error occured are provided. Then, the program halts.

The program will also halt after an error from `lexer` such as "illegal token".

The program uses one and two-character abbreviations internally for the nonterminal symbols, so these must be translated into the longer names before output using `expand-nt-names`. For example, `D` becomes `<Declaration>`. These abbreviations are decoded with a hash table made from the `*nt-key*`.

As the parser works, semantic actions are triggered depending on the productions that come up. This happens in the `main-loop` when a new nonterminal goes to the top of the stack using the function `semantics-NT`. The function `semantics-T` takes care of a few additional actions that must be done at other times, such as right after a "while" statement.

There is an `*op-stack*` used to hold assembly code operators. Usually operands go directly to the assembly code listing as they are parsed. Then the operators go from the stack to the listing using an infix to postfix algorithm that observes proper precedence of operators.

A `check-boolean-arithmetic` function checks to see if boolean variables or constants are used with arithmetic or otherwise used where integers should be used. It works at the end of each statement or operation in parentheses by looking at operations in the assembly listing and seeing if there are integers used, booleans used, and arithmetic being performed. Since only the "integer" and "boolean" types are usable, this function checks for improper use of types. "true" and "false" get converted to 1 and 0 elsewhere, but "true" and "false" are still not permitted to be used in arithmetic or to initialize integers.

The assembly listing of the compiler as written is limited to 500 statements and the symbol table is limited to 100 symbols. These can easily be extended by changing `make-array` in the `*symbol-table*` and `*asm-table*` parameters. 
