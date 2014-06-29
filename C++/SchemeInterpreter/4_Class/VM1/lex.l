%{

#ifndef _MSC_VER
#include <unistd.h>
#else
#define YY_NO_UNISTD_H
#endif

#include "SExpression.h"
#define YYSTYPE SExpression

#include "parser.yy.hpp"

extern int isatty(int n);

%}

Int [0-9]+
Int16 0x[0-9a-fA-F]+
Float {Int}?\.{Int}
IntOrFloat ({Int}|{Float})
EFloat {IntOrFloat}[eE]\-?{IntOrFloat}

%%

[ \t\n\r]+ {}


\"([^\\\"]|\\.)*\" { 
    string s = yytext;
    yylval = SExpression::createString(AtomPool::instance()->intern(unescapeString(s.substr(1, s.size() - 2)).c_str()));
    return STRING;
}

{Int16} {  
    yylval = SExpression::createInt(AtomPool::instance()->intern(yytext));
    return INT; 
}

{Int} { 
    yylval = SExpression::createInt(AtomPool::instance()->intern(yytext));
    return INT; 
}

{Float} { 
    yylval = SExpression::createFloat(AtomPool::instance()->intern(yytext));
    return FLOAT; 
}

{EFloat} { 
    yylval = SExpression::createFloat(AtomPool::instance()->intern(yytext));
    return FLOAT; 
 }

[^(). \r\n]+ {  
    yylval = SExpression::createSymbol(AtomPool::instance()->intern(yytext));
    return SYMBOL; 
}

. { return yytext[0];}

%%

#ifdef _MSC_VER
extern int isatty(int n) {
    return 0;
}
#endif

extern int yywrap() {
    (void)yyunput;
    return 1;
}

extern void yyerror(const char *msg) {
    printf("syntax error (%d) : %s\n", yyget_lineno(), msg);
}
