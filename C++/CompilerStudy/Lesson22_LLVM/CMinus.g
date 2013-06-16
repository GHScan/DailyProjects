grammar CMinus;

options {
    language = Cpp;
    //backtrack = true;
    k = 2;
}

@lexer::header {
#include "pch.h"
}
@lexer::traits {
    class CMinusLexer; 
    class CMinusParser; 
    typedef antlr3::Traits< CMinusLexer, CMinusParser > CMinusLexerTraits;
    typedef CMinusLexerTraits CMinusParserTraits;
}

@parser::header {
#include "pch.h"
#include "CMinusLexer.hpp"
}
@parser::members {

static string unEscape(const string& s) {
    string r;
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[i + 1]) {
            case 'a': r.push_back('\a'); break;
            case 'n': r.push_back('\n'); break;
            case 'r': r.push_back('\r'); break;
            case 't': r.push_back('\t'); break;
            default: r.push_back(s[i + 1]); break;
            }
            ++i;
        } else {
            r.push_back(s[i]);
        }
    }
    return r;
}

}

// parser

program 
options{k=3;}
    : extern_function_declare
    | function_define
    | global_variable_define
    ;

global_variable_define
    : type id_comma_list
    ;

extern_function_declare
    : 'extern' function_head ';'
    ;

function_head
options {k=4;}
    : type ID '(' '...'? ')' 
    | type ID '(' type_id_comma_list (',' '...')? ')' 
    ;

function_define
    : function_head '{' statement* '}'
    ;

statement 
    : ';'
    | expression ';'
    | '{' statement* '}'
    | type variable_init_comma_list ';'
    | 'continue' ';'
    | 'break' ';'
    | 'return' expression? ';'
    | 'if' '(' expression ')' statement (('else')=>'else' statement)?
    | 'for' '(' expression? ';' expression? ';' expression? ')' statement
    | 'while' '(' expression ')' statement
    ;

id_comma_list 
    : ID (',' ID)*
    ;

variable_init_comma_list
    : variable_init (',' variable_init)*
    ;

variable_init
    : ID
    | ID '=' assign_expression
    ;

type_id_comma_list 
    : type_id (',' type_id)*
    ;

type_id : type ID ;

type 
    : 'int' | 'void' | 'float' |'char*'
    ;

expression 
    : assign_expression
    ;

assign_expression 
    : ID '=' assign_expression
    | ID COMBINE_ASSIGN_OP assign_expression
    | logic_expresion
    ;

logic_expresion
    : relat_expression (LOGIC_OP relat_expression)*
    ;

relat_expression
    : add_expression (RELAT_OP add_expression)*
    ;

add_expression
    : mul_expression (('+' | '-') mul_expression)*
    ;

mul_expression
    : unary_expression (('*' | '/' | '%') unary_expression)*
    ;

unary_expression
    : ('-' | '!')? primary_expression 
    ;

primary_expression
    : ID
    | STRING_LITERAL
    | 'true'
    | 'false'
    | call_expression
    | '(' expression ')'
    | ('++' | '--') ID
    | '(' type ')' primary_expression
    ;

call_expression
    : ID '(' expression (',' expression)* ')'
    ;

// lexer
COMBINE_ASSIGN_OP : '+=' | '-=' | '*=' | '/=' | '%=';
RELAT_OP : '==' | '!=' | '<' | '<=' | '>' | '>=';
LOGIC_OP : '&&' | '||';

ID : ID_HEAD_LETTER ID_BODY_LETTER*;
STRING_LITERAL : '"' (~('"' | '\\') | '\\' .)* '"';
NUMBER : DIGIT* ('.' DIGIT*) ?;

COMMENT : '/*' (options{greedy=false;}:.)* '*/' {skip();};
LINE_COMMENT : '//' ~('\r'|'\n')* '\r'?'\n' {skip();};
WS : (' ' | '\t' | '\r'?'\n')+ {skip();};

fragment ID_HEAD_LETTER : ALPHA | '_';
fragment ID_BODY_LETTER : ID_HEAD_LETTER | DIGIT;
fragment ALPHA : 'a'..'z' | 'A'..'Z';
fragment DIGIT : '0' ..'9';
