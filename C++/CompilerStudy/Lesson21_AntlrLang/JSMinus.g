
grammar JSMinus;

options {
    language = Cpp;
    //backtrack = true;
    k = 1;
}

@lexer::header {
#include "pch.h"
}
@lexer::traits {
    class JSMinusLexer; 
    class JSMinusParser; 
    typedef antlr3::Traits< JSMinusLexer, JSMinusParser > JSMinusLexerTraits;
    typedef JSMinusLexerTraits JSMinusParserTraits;
}

@parser::header {
#include "pch.h"
#include "JSMinusLexer.hpp"
}


// syntax

program : statement*  EOF;

statement
    : (leftValue assignOp)=>assignment
    | ('var'? 'function')=>funcDefine
    | ('var' ID P_ASSIGN_OP)=>varDefine
    | rtTypeAtom // '(' exprList? ')'
    | varDeclares
    | '{' statement* '}'
    | 'if' '(' expr ')' statement (('else')=>'else' statement)?
    | 'for' '(' forStart? ';' expr? ';' expr? ')' statement
    | 'while' '(' expr ')' statement
    | 'break'
    | 'continue'
    | 'return' ((expr)=>expr)?
    | ';'
    ;

varDeclares
    : 'var' idList
    ;
varDefine
    : 'var' ID P_ASSIGN_OP expr
    ;

funcDefine 
    : 'var'? 'function' ID funcBody
    ;

funcBody
    : '(' idList? ')' '{' statement* '}'
    ;

forStart
    : assignment
    | varDefine
    ;

idList
    : ID (',' ID)*
    ;

assignment 
    : leftValue assignOp expr
    ;
leftValue
    //: (ID assignOp)=>ID
    : rtTypeAtom //'[' expr ']'
    ;

arrayExpr
    : '[' exprList? ']'
    ;
lambdaExpr
    : 'function' funcBody
    ;

exprList 
    : expr (',' expr)*
    ;
expr
    : logicExpr
    ;

logicExpr
    : relatExpr (('&&' | '||') relatExpr)*
    ;
    
relatExpr
    : addExpr (('<' | '<=' | '>' | '>=' | '==' | '!=') addExpr)*
    ;

addExpr
    : mulExpr (('+' | '-') mulExpr)*
    ;

mulExpr 
    : unaryExpr (('*' | '/' | '%') unaryExpr)*
    ; 

unaryExpr
    : ('not' | '-')? atom
    | ('++' | '--') leftValue
    ;

atom 
    : literalAtom
    | rtTypeAtom
    | '(' expr ')'
    ;

literalAtom
    : NUMBER_LITERAL
    | STRING_LITERAL
    | arrayExpr
    | lambdaExpr
    | 'true'
    | 'false'
    | 'null'
    ;

rtTypeAtomHead
    : ID
    //| lambdaExpr '(' exprList? ')'
    ;
rtTypeAtom
    : rtTypeAtomHead ('(' exprList?  ')'| '[' expr ']')* 
    ;

assignOp
    : P_ASSIGN_OP | C_ASSIGN_OP
    ;
// lex

P_ASSIGN_OP: '=';
C_ASSIGN_OP: '-=' | '+=' | '*=' | '/=' | '%=';
 
ID : ID_START ID_LETTER*;
STRING_LITERAL : '\'' (~('\'' | '\\') | '\\' .)* '\'';
NUMBER_LITERAL : DIGIT+ ('.' DIGIT+)?;

WS : (' ' | '\t' | '\n' | '\r')+ {skip();};
LINE_COMMENT : '//' ~('\r' | '\n')* '\r'? '\n' {skip();};
COMMENT : '/*' (options{greedy=false;}:.)* '*/' {skip();};

fragment ID_START : ALPHA | '_';
fragment ID_LETTER : ID_START | DIGIT;
fragment ALPHA : 'a' .. 'z' | 'A' .. 'Z';
fragment DIGIT : '0' .. '9';
