grammar Expr;

options {
    language = Cpp;
}

@lexer::header {
#include "pch.h"
}
@lexer::traits {
    class ExprLexer; 
    class ExprParser; 
    typedef antlr3::Traits< ExprLexer, ExprParser > ExprLexerTraits;
    typedef ExprLexerTraits ExprParserTraits;
}

@parser::header {
#include "pch.h"
#include "ExprLexer.hpp"
#include <map>
}

@parser::members {
    map<string, int> m_memory;
}

prog : stat+;
stat
    : expr NEWLINE { printf("\%d\n", $expr.value); }
    | ID '=' expr NEWLINE { m_memory[$ID.text] = $expr.value; }
    | NEWLINE 
    ;
expr returns [int value] 
    : a=mulexpr { value = $a.value; } 
    ('+' b=mulexpr { value += $b.value ;}
     | '-' b=mulexpr { value -= $b.value;}
     )*
    ;
mulexpr returns [int value]
    : a=atom  { value = $a.value; }
    ('*' b=atom { value *= $b.value;}
     | '/' b=atom { value /= $b.value;}
     | '%' b=atom { value \%= $b.value; }
     )*
    ;
atom returns [int value]
    : ID { value = m_memory[$ID.text]; }
    | NUMBER { sscanf($NUMBER.text.c_str(), "\%d", &value); }
    | '(' expr ')' { value = $expr.value; }
    ;

ID : ALPHA (ALPHA | DIGIT)*;
NUMBER : DIGIT+;
fragment ALPHA : 'a' .. 'z' | 'A' .. 'Z';
fragment DIGIT : '0' .. '9';
WS : ('\t' | ' ' ) {skip();};
NEWLINE : '\r'? '\n';
