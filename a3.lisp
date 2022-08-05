;;;Program: a3.lisp
;;;An example of a compiler written in lisp (CMUCL) 
;;;The compiler reads source code and produces output in assembly language.
;;;A top-down table-driven predictive parser is used.
;;;The language processed is called "Rat10"; its syntax can be seen in
;;;  the "syntax-rules" file.

;;the 'charlist' below is the index to the proper lextable
;;column by matching incoming chars
;;#\l: any letter  #\d: any digit  #\b: backspace  #\i: illegal char
(defparameter *charlist* (concatenate 'list '(#\l #\d #\_ #\+ #\- #\= #\< #\> #\/ #\* #\:
  #\; #\, #\{ #\} #\( #\) #\[ #\] #\$ #\Space #\b #\i)))

;;the lexer table that contains the state machine
;;see the right hand side "States" column to see what state each input character results in
(defparameter *lextable* (list
 ;     0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21      bsp?   States
 ;    "l""d""_""+""-""=""<"">""/""*"":"";"",""{""}""("")""[""]""$"sp \b \i  
 '(1   2  4 32  7  8  9 11 13 14 16 17 19 20 21 22 23 24 25 26 27 30 31 32   ) ;1  Starting state
 '(2   2  2  2  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3   ) ;2  In word
 '(3   1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 y ) ;3  End of word
 '(4   5  4  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5   ) ;4  In number
 '(5   1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 y ) ;5  End of number
 '(6  32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32   ) ;6  Found _
 '(7   1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;7  Token: +
 '(8   1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;8  Token: -
 '(9  31 31 31 31 31 31 31 10 31 31 31 31 31 31 31 31 31 31 31 31 31 31  1   ) ;9  Found =
 '(10  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;10 Token: =>
 '(11 31 31 31 31 31 12 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31   ) ;11 Found <
 '(12  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;12 Token: <=
 '(13  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1   ) ;13 Token: >
 '(14 31 31 31 31 31 15 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31   ) ;14 Found /
 '(15  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;15 Token: /=
 '(16  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;16 Token: *
 '(17 31 31 31 31 31 18 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31   ) ;17 Found :
 '(18  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;18 Token: :=
 '(19  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;19 Token: ;
 '(20  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;20 Token: ,
 '(21  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;21 Token: {
 '(22  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;22 Token: }
 '(23  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;23 Token: (
 '(24  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;24 Token: )
 '(25  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;25 Token: [
 '(26  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;26 Token: ]
 '(27 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 31 28 31 31 31   ) ;27 Found: $
 '(28  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;28 Token: $$
 '(30  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ) ;30 whitespace
 '(31  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 y ) ;31 Token, not double, backup
 '(32  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 n ))) ;32 illegal token found

(defparameter *state* 1)
(defparameter *source-code* "")
(defparameter *source-ptr* 0)
(defparameter *word* "")
(defparameter *identifier* "")
(defparameter *number* "")
(defparameter *have-token* NIL)
(defparameter *token* "")
(defparameter *lexeme* "")
(defparameter *line-number* 1)

;;make a string to save output to
(defparameter *output-stream* (make-string-output-stream))

(defun cmd-ln-switch (option)
  "checks for command line options"
  (if (position T (mapcar #'
       (lambda (x) (string= option x)) *command-line-words*))
    (return-from cmd-ln-switch T)
    (return-from cmd-ln-switch NIL)))

(defmacro long-output (&body body)
  "Output to both std-out and an output stream 
   called '*output-stream*' which must be present
   when this macro is used. Prints the long
   (spelled out) version of productions"
    `(if (cmd-ln-switch "long")
       (progn
         (format (make-broadcast-stream
            ,*output-stream* *standard-output*) ,@body))))

(defmacro output (&body body)
  "output to both std-out and an output stream 
   called '*output-stream*' which must be present
   when this macro is used. "
     `(format (make-broadcast-stream
         ,*output-stream* *standard-output*) ,@body))

(defun print-word-token (wordtype)
  "print keyword or identifier"
  (if (string-equal wordtype "keyword")
    (progn
      (if (cmd-ln-switch "long")
        (output "Token: Keyword     Lexeme: ~a~%" *word*)
        (if (cmd-ln-switch "debug") (output "Lexeme: ~a~%" *word*)))
      (setf *token* "keyword"))
    (progn
      (if (cmd-ln-switch "long")
        (output "Token: Identifier  Lexeme: ~a~%" *word*)
        (if (cmd-ln-switch "debug") (output "Lexeme: ~a~%" *word*)))
      (setf *token* "identifier")))
  (setf *lexeme* *word*)
  (setf *have-token* T))

(defun print-number-token ()
  "print integers"
  (if (cmd-ln-switch "long")
    (output "Token: Integer     Lexeme: ~a~%" *number*)
    (if (cmd-ln-switch "debug") (output "Lexeme: ~a~%" *number*)))
  (setf *token* "integer")
  (setf *lexeme* *number*)
  (setf *have-token* T))

(defun print-illegal ()
  "print illegal characters"
  (let ((illegal-char (char *source-code* *source-ptr*)))
    ;;handle problem printing '~' to output string stream
    (if (string-equal "~" illegal-char)
        (output "Illegal token \"tilde\"~%")
        (output "Illegal token \" ~a \" on line number ~a.~%"
          illegal-char *line-number*))
    (setf *token* "illegal")
    (setf *lexeme* illegal-char))
    (setf *have-token* T))

(defun print-char-token (token)
  "print out valid token and keep track of $$ (third $$ is EOF)"
  (cond ((member *state* '(7 8 9 10 11 12 13 14 15 16))
           (long-output "Token: Operator    ")
           (setf *token* "operator"))
        ((member *state* '(17 18 19 20 21 22 23 24 25 26 28))
           (long-output "Token: Separator   ")
           (setf *token* "separator"))
        ;;state 31 is for chars that might have been part of
        ;;double char tokens, but are single char tokens
        ((and (eql *state* 31) (member (char *source-code* *source-ptr*)
                                 '(#\= #\< #\/)))
         (long-output "Token: Operator    ")
         (setf *token* "operator")))
  (if (or (cmd-ln-switch "long")(cmd-ln-switch "debug"))
    (output "Lexeme: ~a~%" token))
  (setf *lexeme* token)
  (setf *have-token* T))

(defun may-be-token ()
  "Establish whether char is single-char token or part of
   double-char token. Also check for certain illegals."
  (let ((thischar (char *source-code* *source-ptr*))
        (nextchar (char *source-code* (+ 1 *source-ptr*)))
        (one-char-token NIL)
        (illegal NIL))
    (case thischar
    ((#\=) (if (not (eql nextchar #\>)) (setf one-char-token T)))
    ((#\<) (if (not (eql nextchar #\=)) (setf one-char-token T)))
    ((#\/) (if (not (eql nextchar #\=)) (setf one-char-token T)))
    ((#\:) (if (not (eql nextchar #\=)) (setf one-char-token T)))
    ((#\$) (if (not (eql nextchar #\$)) (setf illegal T))))  ;illegal single '$'
    (if illegal (print-illegal))
    (if one-char-token (print-char-token thischar))))

(defun handle-ending-underscore ()
  "remove any ending '_'s from identifier and back up search-ptr
   so that '_' will be recognized as illegal"
  (loop while (string-equal "_"
     (subseq *identifier* (- (length *identifier*) 1))) do
     (progn
       (setf *identifier* (subseq *identifier* 0 (- (length *identifier*) 1)))
       (decf *source-ptr*)))) ;back up

(defun check-for-keyword ()
  "Check an incoming string of contiguous alphanumerics beginning
   with a letter to see if it is a keyword. If not, it is accepted
   as an identifier.
   Returns: list of (token type, token)"
   (let ((keywords '("int" "boolean" "if" "endif" "write"
                       "read" "while" "true" "false" "else")))
     ;*word* is the incoming string of contiguous alphanumerics
       (dolist (keyword keywords)           ;check for each keyword
         (if (string= *word* keyword)
           (progn
             (if (not (equal #\Newline (char *source-code* *source-ptr*)))
               (decf *source-ptr*))
             (return-from check-for-keyword (cons '"keyword" keyword)))))
       (setf *identifier* *word*)
       (if (not (equal #\Newline (char *source-code* *source-ptr*)))
         (decf *source-ptr*))
       (handle-ending-underscore)
       (return-from check-for-keyword (cons '"Identifier" *identifier*))))

(defun do-state ()
  "Called by lexer. Performs most actions for each state as tokens are
   read from the characters."
  (let ((thischar (char *source-code* *source-ptr*)))
    (cond ((eql *state* 30)())                               ;whitespace state; no op.
          ((eql *state* 1)())                                ;at starting state
          ((eql *state* 2) (setf *word* (concatenate 'string *word*  ;add chars to word
                              (string thischar))))
          ;state 3: a word string is complete; call 'check-for-keyword', which 
          ;returns as 'findings' token type (keyword or indicator) and the lexeme
          ((eql *state* 3) (let ((findings (check-for-keyword)))
                             (if (car findings)            
                               (progn
                                 (setf *word* (cdr findings))
                                 (print-word-token (car findings))
                                 (setf *word* "")))))
          ;;state 4: digits are being read
          ((eql *state* 4) (setf *number* (concatenate 'string *number*
                             (string thischar))))
          ((eql *state* 5) (print-number-token) ;number complete
                           (setf *number* "")
                           (if (not (equal #\Newline (char *source-code* *source-ptr*)))
                             (decf *source-ptr*)))
          ((member *state* '(7 8 13 16 19 20 21 22 23 24 25 26)) ;Single-char token found
                           (print-char-token thischar))
          ((member *state* '(10 12 15 18 28))                    ;2-char token found
                           ;assemble the 2-char token and send it to be printed
                           (let ((lastchar (char *source-code* (- *source-ptr* 1))))
                             (print-char-token (concatenate 'string
                               (string lastchar) (string thischar)))))
          ((member *state* '(9 11 14 17 27))           ;Found char that
                           (may-be-token))             ;may be 2-char token
          ((eql *state* 31)(decf *source-ptr*))        ;backup state
          ((member *state* '(32)) (print-illegal)))))  ;illegal character state
          ;;States 29 ("." received) and 6 ("_") are unused because
          ;;if "_" is received in State 1 it is illegal,
          ;;and numbers with "." are no longer allowed,
          ;;so the machine goes to State 32 immediately.

(defun lexer ()
  "takes characters as input and implements the lextable state machine
   and the function 'do-state', which recognizes valid tokens in the chars"
  (setf *have-token* NIL)
  (loop until *have-token* do
    (let ((thischar (char *source-code* *source-ptr*))
          (column 0)
          (row 0))
      (cond ((member thischar '(#\Space #\Tab #\Newline))   ;got whitespace
               (if (equal #\Newline thischar)
                 (incf *line-number*))
               (setf column #\Space))     ;use lextable 'whitespace' column
            ((alpha-char-p thischar)(setf column #\l))      ;got letter
            ((digit-char-p thischar)(setf column #\d))      ;got digit       
            ((member thischar *charlist*)       ;got legal non-alphanum character
               (setf column thischar))
            ((not(member thischar *charlist*))  ;got illegal character
               (setf column #\i)))
      ;;look up state in lextable; row is current state,
      ;;number in column (column based on character) will give next state"
      (setf row *state*)
      (setf *state* (nth (+ 1 (position column *charlist*))
                   (nth (- row 1) *lextable*)))
      (do-state)                 ;do processing for the new state
      ;; finish accepting states by returning to state 1
      (if (member *state* '(3 5 7 8 10 12 13 15 16 18 19 20
                            21 22 23 24 25 26 28 30 31 32))
            (setf *state* 1))
      (incf *source-ptr*))))

(defun prompt-read (prompt)
  "gets input from the user"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)  
  (read-line *query-io*))

(defun show-syntax-error (nt-name-table)
  "When a syntax error occurs, if top of stack is a nonterminal,
   show input lexeme and lexemes that would have been accepted
   based on that nonterminal. If top of stack is a terminal,
   show the input lexeme and expected lexeme. Also show the stack
   and line number of the error. Then halt at that point."
  (let ((nt-row (position (intern (car *the-stack*)) *nonterminals*))
        (expected NIL))
    (output "ERROR: Syntax Error on line ~a. " *line-number*)
    (if nt-row
      (progn   ;nonterminal
        ;; resolve NTs in the stack to expected terminals
        ;; based on the 'parser' and 'identifier'  tables
        (loop for i from 1 to (length *nonterminals*) do
          (if (nth i (nth nt-row *parser-table*))
            (push (nth (- i 1)(car *parser-table*)) expected)))
        (loop for i from 1 to (length *nonterminals*) do
          (if (nth i (nth nt-row *identifier-table*))
            (push (nth (- i 1)(car *identifier-table*)) expected)))
        (setf expected (reverse expected))
        (setf expected (expand-nt-names expected nt-name-table))
        (output "Token was \" ~a \", ~%" *lexeme*)
        (output "expected one of \" ~a \"~%" expected))
      (progn   ;terminal
        (output "  ~a  where ~a was expected.~%"
           *lexeme* (expand-nt-names (list (car *the-stack*)) nt-name-table)))))
  (setf *errors* (append *errors* '("syntax error")))
  (error ""))

(defun expand-nt-names (lexeme-list nt-name-table)
  "Take a list of lexemes and expand nonterminal names that were
   abbreviated for tables and development."
  (let ((longname "")
        (printlist nil))
    (dolist (lex lexeme-list)
      (setf longname (gethash (intern lex) nt-name-table)) 
      (if (not longname)
        (setf printlist (concatenate 'string printlist " " lex))
        (setf printlist (concatenate 'string printlist " " (string longname)))))
        (return-from expand-nt-names printlist)))

(defun get-filenames ()
  "get source filename  and output filename from the user"
  (let ((sourcefile "") (outfile ""))
  (if (cmd-ln-switch "sample")
    (setf sourcefile "sample.rat")
    (progn
      (format t "~%Please enter the source code file name,~%")
      (setf sourcefile (prompt-read "or just hit Enter to quit."))
      (if (or (string-equal "q" sourcefile) (string-equal "" sourcefile))
        (return-from get-filenames NIL))))
    (format t "~%Please enter the output file name,~%")
    (format t "or hit 'Enter' for the default ('source-file-name.out'),~%")
    (setf outfile (prompt-read "or enter q to quit."))
    (cond ((string-equal "q" outfile) (return-from get-filenames NIL))
          ((string-equal "" outfile) (setf outfile (concatenate 'string sourcefile ".out")))
          (T ()))
    ;;save the source file into a string and close it
    (with-open-file (stream sourcefile)
        (setf *source-code* (make-string (file-length stream)))
        (read-sequence *source-code* stream))
    (return-from get-filenames outfile)))

;;table of abbreviations for nonterminals
(defparameter *nt-key* (list
'((Z) ("<Rat10>"))
'((L) ("<Declaration List>"))
'((LP)("<Opt Declaration List>"))
'((M) ("<Statement List>"))
'((MP)("<Statement List Prime>"))
'((Q) ("<Qualifier>"))
'((D) ("<Declaration>"))
'((I) ("<IDs>"))
'((IP)("<IDs Prime>"))
'((S) ("<Statement>"))
'((Y) ("<Compound>"))
'((A) ("<Assign>"))
'((V) ("<If>"))
'((G) ("<Write>"))
'((H) ("<Read>"))
'((W) ("<While>"))
'((C) ("<Condition>"))
'((R) ("<Relop>"))
'((E) ("<Expression>"))
'((EP)("<Expression Prime>"))
'((T) ("<Term>"))
'((TP)("<Term Prime>"))
'((F) ("<Factor>"))
'((P) ("<Primary>"))
'((PP)("<Primary Prime>"))
'((&) ("<Empty>"))
'((ID)("<Identifier>"))
'((IN)("<Integer>"))
'((RL)("<Real>"))
'((@) ("<End of Stack>"))))

;; table of right-hand-sides for table-driven parser
;; productions too long to fit in the table are coded by a number and found in the *rhs-list*
(Defparameter *identifier-table* (list
 ;    0         1        2      3     4      5       6      7       8        9      10    
'(   "ID"    "int" "boolean"  "if"  "else" "endif" "write" "read" "while" "true" "false" )
'(Z  NIL)                    ; note: "&" is epsilon
'(LP ("&")   ("L")  ("L")    ("&")  NIL    NIL     ("&")  ("&")  ("&") )
'(L  NIL      20     20 ) 
'(MP ("M")    NIL    NIL     ("M")  NIL    NIL     ("M")   ("M")   ("M") )
'(M ("S""MP") NIL    NIL    ("S""MP") NIL  NIL   ("S""MP")("S""MP")("S""MP") )
'(Q  NIL    ("int")("boolean"))
'(D  NIL    ("Q""I")("Q""I"))
'(I ("ID""IP") )
'(IP  )
 ;    0      1        2      3     4      5       6      7       8        9      10    
'(S ("A")   NIL     NIL     ("V")    NIL   NIL   ("G")   ("H")    ("W") )
'(Y )
'(A   5 )
'(V  NIL    NIL     NIL     ("if""(""C"")""S""VP") )
'(VP NIL    NIL     NIL      NIL   24   ("endif") )
;;   "ID"  "int" "boolean" "if" "else" "endif" "write"  "read"   "while" "true" "false"
'(G  NIL    NIL     NIL      NIL   NIL    NIL     8 )   
'(H  NIL    NIL     NIL      NIL   NIL    NIL    NIL      9 )
'(W  NIL    NIL     NIL      NIL   NIL    NIL    NIL     NIL      10 )
'(C  11     NIL     NIL      NIL   NIL    NIL    NIL     NIL      NIL     11      11 )
'(R )                                      
'(E ("T""EP") NIL   NIL      NIL   NIL    NIL    NIL     NIL      NIL   ("T""EP")("T""EP") )
'(EP )
'(T ("F""TP") NIL   NIL      NIL   NIL    NIL    NIL     NIL      NIL   ("F""TP")("F""TP") )
'(TP ) 
'(F ("P")    NIL    NIL      NIL   NIL    NIL    NIL     NIL      NIL     ("P")   ("P") )
'(P ("ID""PP") NIL  NIL      NIL   NIL    NIL    NIL     NIL      NIL   ("true")("false") )
;;   "ID"   "int" "boolean" "if" "else" "endif" "write" "read"   "while" "true" "false"
'(PP )
'(@ )
))

;; second table of right-hand-sides for table-driven parser
;;identifiers and keywords use the 'identifier-table' above instead of this one
;; productions too long to fit in the table are coded by a number and found in the *rhs-list*
(defparameter *parser-table* (list
 ;   0   1    2    3   4   5   6   7   8   9   10  11  12  13  14   15  16   17  18  19  20  21  
'( "IN" "$$" "[" "]" "," ":" "{" "}" ";" "(" ")" "=" "/=" ">" "<" "=>" "<=" "+" "-" "*" "/" ":=")
'(Z NIL  0 )                            ; note: "@" is top-of-stack
'(LP NIL NIL  NIL NIL NIL NIL 18 )      ; note: 18 is epsilon
'(L )
'(MP NIL 18  NIL NIL  NIL  NIL  4  18 )
'(M NIL  NIL  NIL NIL NIL  NIL  3 NIL )
'(Q )
'(D )
'(I )
'(IP NIL NIL NIL  18   21  18  NIL NIL 18 NIL 18 NIL  NIL  NIL NIL NIL NIL  NIL NIL  NIL NIL  18)
'(S NIL  NIL  NIL NIL  NIL NIL ("Y") )
'(Y NIL  NIL  NIL NIL  NIL NIL 4 )
'(A )
'(V )
'(VP )
;;   "IN""$$" "[" "]" "," ":" "{" "}" ";" "(" ")" "=" "/=" ">" "<" "=>" "<=" "+" "-" "*" "/" ":="
'(G )
'(H )
'(W )                                ;note: 18 is epsilon
'(C   11 NIL NIL NIL NIL  NIL NIL NIL NIL  11 NIL  NIL NIL  NIL NIL NIL  NIL  NIL  11 )
'(R  NIL NIL NIL NIL NIL  NIL NIL NIL NIL NIL NIL("=")("/=")(">")("<")("=>")("<=") )
'(E  12  NIL NIL NIL NIL  NIL NIL NIL NIL 12 NIL  NIL NIL  NIL NIL NIL  NIL  NIL  12 )
'(EP NIL NIL NIL NIL NIL  NIL NIL NIL 18  NIL 18  18  18   18  18  18   18   13   14 )
'(T  15  NIL NIL NIL NIL  NIL NIL NIL NIL 15  NIL NIL NIL  NIL NIL NIL  NIL  NIL  15 )
'(TP NIL NIL NIL NIL NIL  NIL NIL NIL 18  NIL 18  18  18   18  18  18   18   18   18 16  17 )
'(F ("P")NIL NIL NIL NIL  NIL NIL NIL NIL("P")NIL NIL NIL NIL NIL  NIL  NIL  NIL ("-""P") )
'(P ("IN")NIL NIL NIL NIL NIL NIL NIL NIL 19 )
'(PP NIL NIL  22 NIL NIL  NIL NIL NIL 18 NIL  18  18   18  18  18   18  18   18  18  18  18 )
'(@ )
;;  "IN""$$" "[" "]" "," ":"  "{" "}" ";" "(" ")" "=" "/=" ">" "<" "=>" "<=" "+" "-" "*" "/" ":="
))

;; this table is for productions that are too long to fit well in the tables above
;; so they are coded by a number. The numbers appear in the tables.
(defparameter *rhs-list* (list 
;  0                               1                2
'("$$" "$$" "LP" "M" "$$") '("no longer used") '("I"":""Q")

;    3           4           5            6                  
  '("S""MP") '("{""M""}") '("ID"":=""E"";") '("if""(""C"")""S""endif")
;  7               8                      9                
 '("E"";") '("write""(""E"")"";") '("read""(""I"")"";")

;         10
 '("while""(""C"")""S""JMP")
 
;  11              12        13          14          15        
 '("E""R""E") '("T""EP") '("+""T""EP") '("-""T""EP") '("F""TP")
;  16              17         18      19           20
 '("*""F""TP")'("/""F""TP") '("&") '("(""E"")") '("D"";""LP")
;     21         22          23                   24
  '(",""I") '("[""I""]") '("no longer used") '("else""S""endif")))

;;read nonterminals from *parser-table*
(defparameter *nonterminals* (mapcar #' (lambda (x) (car x)) *parser-table*))
(defparameter *the-stack* NIL)
(defparameter *symbol-table* (make-array '(100 3) :adjustable T :initial-element ""))
(defparameter *asm-table* (make-array '(500 3) :adjustable T :initial-element "" ))
(defparameter *next-asm-row* 1)  ; assembly code starts with line 1
(defparameter *assignment* NIL)  ; flag for when current statement is assignment
(defparameter *declaration* NIL) ; flag for when current statement is declaration
(defparameter *left-side* NIL)   ; flag for left side of assignment ':='
(defparameter *errors* ())
(defparameter *unary* NIL)
(defparameter *true-false* NIL)
(defparameter *backpatch* NIL)
(defparameter *read* NIL)
(defparameter *write* NIL)
(defparameter *op-stack* NIL)

(defun print-symbol-table ()
  "prints symbol table"
  (let ((row 0)(left "nothing")(middle 0)(right "nothing"))
    (loop until (string= "" left) do
      (setf left (aref *symbol-table* row 0))
      (setf middle (aref *symbol-table* row 1))
      (setf right (aref *symbol-table* row 2))
      (output "~a ~a ~a~%" left middle right)
      (incf row))))

(defun print-asm-table ()
  "prints assembly listing"
  (let ((row 1)(left 0)(middle "nothing")(right "nothing"))
    (loop until (string= "" middle) do
      (setf left (aref *asm-table* row 0))
      (setf middle (aref *asm-table* row 1))
      (setf right (aref *asm-table* row 2))
      (output "~a ~a ~a~%" left middle right)
      (incf row))))

(defun find-variable (name)
  "see if variable name is in symbol table
   if so, return table row and variable type"
  (let ((row 0)(this-one "nothing"))
    (loop until (string= "" this-one) do
      (setf this-one (aref *symbol-table* row 0))
      (if (string= name this-one)
        (return-from find-variable (list row (aref *symbol-table* row 2))))
      (incf row))
    (return-from find-variable NIL)))

(defun find-address (number)
  "see if address is in symbol table
   if so, return table row and variable type"
  (let ((row 0)(this-name "nothing")(this-addr 0))
    (loop until (string= "" this-name) do
      (setf this-name (aref *symbol-table* row 0))
      (setf this-addr (aref *symbol-table* row 1))         
      (if (equal number this-addr)
        (return-from find-address (list row (aref *symbol-table* row 2))))
      (incf row))
    (return-from find-address NIL)))

(defun add-code (op1 op2) 
  "put an operation on the *asm-table* 
   *asm-table* columns:  row number - operator - operand"
  (setf (aref *asm-table* *next-asm-row* 0) *next-asm-row*)
  (setf (aref *asm-table* *next-asm-row* 1) op1)
  (setf (aref *asm-table* *next-asm-row* 2) op2)
  (if (cmd-ln-switch "debug")
    (output "~a   ~a   ~a~%~a~% ~a~%" *next-asm-row* op1 op2 *the-stack* *op-stack*))
  (incf *next-asm-row*))
  
(defun check-boolean-arithmetic ()
  "check for illegal arithmetic with booleans"
  (let ((op "")(have-arith)(have-bool)(have-ints)
        (this-row (- *next-asm-row* 1)))
    (if *true-false* (setf have-bool T))  ; using "true" or "false" in stmt
    (if (string= "POPM" (aref *asm-table* this-row 1))
      (if (string= "boolean" (second (find-address
                                         (aref *asm-table* this-row 2))))
        (setf have-bool T)    ;storing a boolean
        (setf have-ints T)))  ;storing an int
    ;;looking at the operations in this statement or parentheses grouping
    ;; depending on where the function was called from
    (loop for row downfrom (- *next-asm-row* 2) downto 1 do
      (setf op (aref *asm-table* row 1))
      ;;still in the statement while these are used   
      (when (not (position T (mapcar #' (lambda (x) (string= x op))
                  '("ADD" "SUB" "MUL" "DIV" "PUSHI" "PUSHM"))))
        (return-from check-boolean-arithmetic))
      ;;see if statement uses arithmetic   
      (if (position T (mapcar #' (lambda (x) (string= x op))
           '("ADD" "SUB" "MUL" "DIV"))) (setf have-arith T)) ; using arithmetic
      (if (string= "PUSHM" (aref *asm-table* row 1))
        (if (string= "boolean" (second (find-address (aref *asm-table* row 2))))
          (setf have-bool T)   ;using a boolean variable
          (setf have-ints T))) ;using an int variable
      (if (and (string= "PUSHI" (aref *asm-table* row 1))
               (not (equal 0 (aref *asm-table* row 2)))
               (not (equal 1 (aref *asm-table* row 2))))
          (setf have-ints T))  ;using an integer constant
;     (format t "have bool: ~a    have ints: ~a   have arith: ~a~%"
;        have-bool have-ints have-arith)
      (if (and have-bool have-ints)
        (progn
          (output "ERROR: Boolean used with integers ")
          (output "in line ~a.~%" *line-number*)               
          (setf *errors* (append *errors* '("booleans used with integers")))
          (error "")))
      (if (and have-bool have-arith)
        (progn
          (output "ERROR: Boolean used in arithmetic expression ")
          (output "in line ~a.~%" *line-number*)               
          (setf *errors* (append *errors* '("boolean arithmetic")))
          (error "")))
      (setf *true-false* NIL))))

(defun push-op-stk (code)
  "Pushes a code on the 'operation stack'."
  (push code *op-stack*)
  (if (cmd-ln-switch "debug")(output "~a -> op-stack~%" code)))

(let ((negate-relop)(next-symbol-addr 1000)(next-symbol-row 0)(type)) ; define closures
  (defun semantics-NT ()
    "Do semantic actions for each production and set up for semantic actions
     that will be done in 'semantics-T' as lexemes come in"
    (cond
      ((string= "A" (car *the-stack*))  ; 'assignment' production
        (setf *assignment* T)
        (setf *left-side* T)
        (setf *true-false* NIL)
        (if (string= "identifier" *token*)
          (progn
            (push-op-stk (aref *symbol-table* (car (find-variable *lexeme*)) 1))
            (push-op-stk "POPM"))))
      ((string= "D" (car *the-stack*))  ; 'declaration' production
        (setf *declaration* T)
        (cond
          ((or (string= "int" *lexeme*)(string= "boolean" *lexeme*))
             (setf type *lexeme*))))
      ((string= "F" (car *the-stack*))  ; 'factor' production
        ;;handle unary minus; continued in "P" below
        (if (string= "-" *lexeme*)
          (progn
            (add-code "PUSHI" -1)
            (setf *unary* T))))
      ((string= "TP" (car *the-stack*)) ; 'term-prime' production
         (cond
           ((string= ")" *lexeme*)
             ;;pop items off of *op-stack* to assembly listing
             (let ((item "")(addr ""))
               (loop
                 (setf item (pop *op-stack*) addr "" )
                 (if (cmd-ln-switch "debug")              
                   (output "op-stk -> ~a ~%" item))              
                 (when (or (string= "(" item) (equal NIL item)) (return)) ; quit at "("
                 (if (string= "POPM" item)(setf addr (pop *op-stack*)))
                 (cond
                   ((string= "*" item)(add-code "MUL" ""))
                   ((string= "/" item)(add-code "DIV" ""))
                   ((string= "+" item)(add-code "ADD" ""))
                   ((string= "-" item)(add-code "SUB" ""))                 
                   (T (add-code item addr)))))
             (check-boolean-arithmetic)
             ;;set up jumps after compare
             (if (and *backpatch* (not *write*) (not *assignment*))
               (if negate-relop  ; if '=>', '/=', or '<='
                 (progn
                   (add-code "JUMPZ" (+ 2 *next-asm-row*))
                   (add-code "JUMP" -998)
                   (setf negate-relop NIL))
                 (progn
                   (format t "putting jumpz~%")
                   (add-code "JUMPZ" -998)))))  ; -998 means 'backpatch here' 
            ((or (string= "*" *lexeme*)(string= "/" *lexeme*))
              ;;convert infix to postfix and save assignment variable for last
              (let ((item "")(temp-stk))
                (dotimes (x (length *op-stack*))
                  (setf item (pop *op-stack*))
                  (cond
                    ((string= "POPM" item)
                       (push item temp-stk)
                       (push (pop *op-stack*) temp-stk))
                    ((string= "(" item)
                        (push "(" *op-stack*)
                       (return))
                    ((or (string= "+" item)(string= "-" item))
                       (push item temp-stk))
                    ((string= "*" item)(add-code "MUL" ""))
                    ((string= "/" item)(add-code "DIV" ""))
                    (T (push item temp-stk))))
                (dotimes (x (length temp-stk))
                  (push (pop temp-stk) *op-stack*)))
              (push *lexeme* *op-stack*)
              (format t "op-stack at TP: ~a~%" *op-stack*))))
      ((string= "EP" (car *the-stack*))  ; 'expression-prime' production
         (if (or (string= "+" *lexeme*)(string= "-" *lexeme*))
           ;;convert infix to postfix and save assignment variable for last
           (let ((item "")(temp-stk))
             (dotimes (x (length *op-stack*))
               (setf item (pop *op-stack*))
               (cond
                 ((string= "POPM" item)
                    (push item temp-stk)
                    (push (pop *op-stack*) temp-stk))
                 ((string= "(" item)(push "(" *op-stack*)(return))
                 ((string= "*" item)(add-code "MUL" ""))
                 ((string= "/" item)(add-code "DIV" ""))
                 ((string= "+" item)(add-code "ADD" ""))
                 ((string= "-" item)(add-code "SUB" ""))
                 (T (push item temp-stk))))
             (dotimes (x (length temp-stk))
               (push (pop temp-stk) *op-stack*))
               (push *lexeme* *op-stack*))))     
      ((string= "P" (car *the-stack*)) ; 'primary' production
         (cond
           ((string= "identifier" *token*)
              ;;check that identifiers are declared
              (if (and (string= "identifier" *token*)(not *declaration*)
                  (not (find-variable *lexeme*)))
                (progn
                  (output "ERROR: Variable '~a' on line number ~a was not declared.~%"
                     *lexeme* *line-number*)
                  (setf *errors* (append *errors* '("variable not declared")))
                  (error "")))
            
              (add-code "PUSHM" (aref *symbol-table* (car (find-variable *lexeme*)) 1))
             (if *unary* (progn (add-code "MUL" "")(setf *unary* NIL))))           
           ((string= "integer" *token*)
             (add-code "PUSHI" *lexeme*)
             (if *unary* (progn (add-code "MUL" "")(setf *unary* NIL))))           
           ((string= "(" *lexeme*)(push "(" *op-stack*))
           ((string= "true" *lexeme*)(add-code "PUSHI" 1)(setf *true-false* T))
           ((string= "false" *lexeme*)(add-code "PUSHI" 0)(setf *true-false* T))))
      ((string= "V" (car *the-stack*)) ; 'if' statement production
         (if (string= "if" *lexeme*)
           (setf *backpatch* T)))
      ((string= "VP" (car *the-stack*))
         (if (string= "else" *lexeme*)
            ;;backpatch for 'if' statement with 'else'
            ;; this jump will jump over the else clause
            ;; its address will be filled in when 'endif' is received
            (progn
            (add-code "JUMP" -998) 
            ;; fill in the address for the jump from compare to else clause      
            (loop for row downfrom (- *next-asm-row* 2) downto 1 do
              (if (equal -998 (aref *asm-table* row 2))
                  (progn
                    (setf (aref *asm-table* row 2) *next-asm-row*)
                    (return)))))))
      ((string= "W" (car *the-stack*)) ; 'while' statement production
         (setf *backpatch* T)
         (add-code "LABEL" -997))
      ((string= "R" (car *the-stack*)) ; 'relop' production
         (cond
           ((string= "=" *lexeme*)(push-op-stk "EQU"))
           ((string= "<" *lexeme*)(push-op-stk "LES"))
           ((string= ">" *lexeme*)(push-op-stk "GRT"))
           ((string= "=>" *lexeme*)(setf negate-relop T)(push-op-stk "LES"))    
           ((string= "/=" *lexeme*)(setf negate-relop T)(push-op-stk "EQU"))
           ((string= "<=" *lexeme*)(setf negate-relop T)(push-op-stk "GRT"))))
      ((string= "H" (car *the-stack*))  ; 'read' production
         (setf *read* T))
      ((string= "I" (car *the-stack*))  ; 'identifier' production
         ;;check that identifiers are declared
         (if (and (string= "identifier" *token*)(not *declaration*)
                  (not (find-variable *lexeme*)))
           (progn
             (output "ERROR: Variable '~a' on line number ~a was not declared.~%"
                     *lexeme* *line-number*)
             (setf *errors* (append *errors* '("variable not declared")))
             (error "")))         
         (cond
           (*read*
             (progn
               (add-code "PUSHSTD" "")
               (add-code "POPM" (aref *symbol-table*
                                   (car (find-variable *lexeme*)) 1))))
           (*declaration*
             (if (find-variable *lexeme*) ; check for second declaration
               (progn
                 (output "ERROR: Variable '~a' declared a second time " *lexeme*)
                 (output "on line ~a.~%" *line-number*)
                 (setf *errors* (append *errors* '("variable declared more than once"))))
               (progn ; put a newly declared symbol in the table
                 (setf (aref *symbol-table* next-symbol-row 0) *lexeme*)
                 (setf (aref *symbol-table* next-symbol-row 1) next-symbol-addr)
                 (setf (aref *symbol-table* next-symbol-row 2) type)
                 (if (cmd-ln-switch "debug")
                   (output "~a   ~a   ~a~%" *lexeme* next-symbol-addr type))
                 (incf next-symbol-addr)
                 (incf next-symbol-row))))))
      ((string= "G" (car *the-stack*))  ; 'write' production
         (if (string= "write" *lexeme*)
           (setf *write* T))))))

(defun semantics-T (jmp) 
  "Do semantic actions not triggered by a nonterminal at the top of stack"
    (cond
      ;;do JUMP to beginning and backpatch for a WHILE statement
      ;;also, avoid LABELs already matched to by a previous JUMP
      ;;so that nested WHILEs will work
      (jmp
        (loop for row downfrom (- *next-asm-row* 1) downto 1 do
          (if (and (string= "LABEL" (aref *asm-table* row 1))
                     (equal -997 (aref *asm-table* row 2)))
            (progn
              (add-code "JUMP" row)
              (setf (aref *asm-table* row 2) "")            
              (return))))
        (loop for row downfrom (- *next-asm-row* 1) downto 1 do
          (if (equal -998 (aref *asm-table* row 2)) ; -998 means 'backpatch here'
            (progn
              ;;replace '-998' with the next assembly code line address
              (setf (aref *asm-table* row 2) *next-asm-row*)
              (return)))))
      ;;differentiate the left hand side variable in assignments
      ((string= ":=" *lexeme*)(setf *left-side* NIL))
      ((string= "endif" *lexeme*)
         ;;backpatch for 'if' statement
         (progn
           (format t "processing 'endif' backpatch~%")
           (loop for row downfrom (- *next-asm-row* 1) downto 1 do
             (if (equal -998 (aref *asm-table* row 2))
               (progn
                 (setf (aref *asm-table* row 2) *next-asm-row*)
                 (return))))))
      ((string= ";" *lexeme*)
         (let ((item)(addr))
           ;;end of statement; pop the operations off the *op-stack* 
           (loop 
             (setf item (pop *op-stack*)   addr "")
             (if (cmd-ln-switch "debug")              
               (output "op-stk -> ~a ~%" item))
             (when (not item) (return)) ; quit when stack is empty
             (if (string= "POPM" item)(setf addr (pop *op-stack*)))
             (cond
               ((string= "*" item)(add-code "MUL" ""))
               ((string= "/" item)(add-code "DIV" ""))
               ((string= "+" item)(add-code "ADD" ""))
               ((string= "-" item)(add-code "SUB" ""))                 
               (T (add-code item addr)))))              
         (check-boolean-arithmetic)
         (if *write* (add-code "POPSTD" ""))
         ;; clearing flags at end of statement ";"
         (setf *declaration* NIL   *assignment* NIL   *backpatch* NIL)
         (setf *read* NIL   *write* NIL   *true-false* NIL))))

(loop named main-loop do
  (setf *state* 1)
  (setf *source-code* "")
  (setf *source-ptr* 0)
  (setf *word* "")
  (setf *identifier* "")
  (setf *number* "")
  (let ((outfile (get-filenames)))
    (if (not outfile) (return-from main-loop))
    ;;put starting symbol 'Z' and end of stack symbol '@' on stack
    ;;"Z" (Z=Rat10) and other NT abbreviations will be expanded before output.
    (setf *the-stack* '("Z" "@"))
    (lexer) ; get the first *lexeme* from *source-code*
    (let ((tos "")
          (table "")
          (column 0)
          (row 0)
          (lexeme-or-id "")
          (new-stack-items ())
          (nt-name-table (make-hash-table)))
      ;; make a hashtable from 'nt_key' file
      ;;  to spell out abbreviated nonterminal names for output
      (dolist (nt-name *nt-key*)
        (setf (gethash (caar nt-name) nt-name-table) (caadr nt-name)))
      ;; parse until the end of stack "@" is reached
      (loop named parse until (string-equal "@" (car *the-stack*)) do
         (setf tos (car *the-stack*)) ;get TOS (top of stack)
         (if (not (position (intern tos) *nonterminals*))
           (progn  ; TOS has a terminal; don't need table yet               
             ;;see if TOS matches exact terminal or terminal type reported by lexer
             (if (or (string= tos *lexeme*)
                     (and (string= "ID" tos) (string= "identifier" *token*))
                     (and (string= "IN" tos) (string= "integer" *token*))
                     (and (string= "boolean" tos) (string= "keyword" *token*)))
               (progn ;it matched, so call assembly code maker and pop *the-stack*
                 (semantics-T NIL) ;NEW FOR ASSIGNMENT 3
                 (pop *the-stack*)
                 (if (string= "@" (car *the-stack*)) ; check for top of stack "@"
                   (progn
                     (if *errors*
                       (progn  
                         (output "~%Source file parsing finished with the following errors:~%~%")
                         (mapcar #' (lambda (x) (format t "~a~%" x)) *errors*)
                         (output "~%"))
                       (output "~%Source file parsing finished with no errors.~%~%"))
                     (return-from parse)))
                 (lexer))
               (progn ;it didn't match the expected lexeme;
                      ;either it's a syntax error or a special assembly code "JUMP" flag
                 (if (string= tos "JMP")
                   (progn
                     (semantics-T T)
                     (pop *the-stack*))
                   (progn
                     (show-syntax-error nt-name-table)
                     (setf *errors* (append *errors* '("expected lexeme didn't match top of stack")))
                     (error ""))))))
           (progn  ; TOS has a nonterminal; go to tables for new stack items
             (setf row (position (intern tos) *nonterminals*))
             ;; table is two parts for convenience, separated by token type
             (if (or (string= "identifier" *token*) (string= "keyword" *token*))
               (setf table *identifier-table*) ; this table has ID and keywords
               (setf table *parser-table*))    ; this one has everything else
             ;; use a column code for identifiers and integers
             (cond ((string= "identifier" *token*) (setf lexeme-or-id "ID"))
                   ((string= "integer" *token*) (setf lexeme-or-id "IN"))
                   (T (setf lexeme-or-id *lexeme*)))
             ;; get column by code or lexeme itself at top of column
             (setf column (+ 1 (position T (mapcar #'
               (lambda (x) (string= lexeme-or-id x)) (car table)))))
             ;; look up new stack items in the table       
             (let ((next-rhs (nth column (nth row table)))
                   (productions ()))
               (cond ((or (equal 18 next-rhs) (equal '("&") next-rhs))
                        (setf new-stack-items ())          ;'18' or '&' = epsilon
                        (setf productions '("&")))
                     ((integerp next-rhs)                  ; get long RHSs coded by numbers
                        (setf new-stack-items (nth next-rhs *rhs-list*))
                        (setf productions new-stack-items))
                     ((and (listp next-rhs) (not (equal nil next-rhs)))    
                        (setf new-stack-items next-rhs)    ; get other RHSs directly in tables
                        (setf productions new-stack-items))
                     (T  (show-syntax-error nt-name-table)))  ;empty or none of above, show error
               (if (or (cmd-ln-switch "short")(cmd-ln-switch "debug"))
                   (output "~a -> ~{~a ~}~%" (car *the-stack*) productions)) ; abbreviated version
               ;; output productions, pop stack, push new tokens from table
               (long-output "~a -> " (gethash (intern (car *the-stack*)) nt-name-table))
               (long-output "~a~%" (expand-nt-names productions nt-name-table))
               (semantics-NT) ;NEW FOR ASSIGNMENT 3
               (pop *the-stack*)
               (setf *the-stack* (append new-stack-items *the-stack*)))))))
    (output "Assembly Code Listing~%~%")
    (print-asm-table)
    (output "Symbol Table~%~%")
    (print-symbol-table)
    ;;finished with this file; write the output string to the output file
    ;;an existing file with the same name will be moved to a '.BAK' file
    (with-open-file (outstream outfile
                       :direction :output :if-exists :rename)
      (format outstream (get-output-stream-string *output-stream*)))))
(if (not (cmd-ln-switch "noquit"))
  (quit))
