grammar Expr;

options {
    output = AST;
    ASTLabelType = CommonTree;
}

prog : (stat {System.out.println($stat.tree.toStringTree());} )+;
stat
    : expr NEWLINE -> expr 
    | ID '=' expr NEWLINE -> ^('=' ID expr)
    | NEWLINE -> 
    ;
expr 
    : mulexpr (('+'^ | '-'^) mulexpr)*
    ;
mulexpr
    : atom (('*'^ | '/'^ | '%'^) atom)*
    ;
atom
    : ID
    | NUMBER
    | '('! expr ')'!
    ;

ID : ALPHA (ALPHA | DIGIT)*;
NUMBER : DIGIT+;
fragment ALPHA : 'a' .. 'z' | 'A' .. 'Z';
fragment DIGIT : '0' .. '9';
WS : ('\t' | ' ' ) {skip();};
NEWLINE : '\r'? '\n';
