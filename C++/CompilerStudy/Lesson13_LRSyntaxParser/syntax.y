Start: Funcs
     ;

Funcs : Funcs Func
      | Func ;

Func: IDHead ID "(" IDTuple ")" "{" Stmts "}"
    ;

Stmt: Exp "@"
    | "{" Stmts "}"
    | "@"
    ;

Stmts: Stmts Stmt
      | Stmt
      ;

Exp : Assign
    ; 

Assign : LAssign Add;
LAssign : LAssign ID "="
        |
        ;

Add : Add "+" Mul
    | Add "-" Mul
    | Mul
    ;
Mul : Mul "*" Term
    | Mul "/" Term
    | Mul "%" Term
    | Term
    ;

Term : Int
     | ID
     | "(" Exp ")"
     | Call
     ;

Call : ID "(" ExpTuple ")" ;

ExpTuple: ExpTuple "," Exp
        | Exp
        |
        ;

IDTuple: IDTuple "," ID
       | ID
       |
       ;

ID : IDHead IDBody 
   | IDHead
   ;
IDBody : IDBody IDTail 
       | IDTail
       ;
IDTail : IDHead | Digit;
IDHead : Alpha | "_";
Alpha : "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" 
      | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" 
      ;

Int : Int Digit 
    | Digit
    ;
Digit : "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
