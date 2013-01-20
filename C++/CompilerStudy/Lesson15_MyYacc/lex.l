%{
#define YYSTYPE string
#include "LexGlobal.h"
#include "syntax.yy.h"

%}


%%

"//"[^\n]*\n {
    yyset_lineno(yyget_lineno() + 1);
    // printf("comments : %s\n", yytext);
}
"/*"([^\*]|\*[^/])*"*/" {
    int lineN = 0;
    for (int i = 0; i < yyleng; ++i) lineN += yytext[i] == '\n';
    yyset_lineno(yyget_lineno() + lineN);
    // printf("comments : %s\n", yytext);
}

\s {
    if (yytext[0] == '\n') {
        yyset_lineno(yyget_lineno() + 1);
    }
}

"if" {
    yylval = yytext;
    return ESS_IF;
}
"else" {
    yylval = yytext;
    return ESS_ELSE;
}
"for" {
    yylval = yytext;
    return ESS_FOR;
}
"while" {
    yylval = yytext;
    return ESS_WHILE;
}
"do" { 
    yylval = yytext;
    return ESS_DO;
}
"break" {
    yylval = yytext;
    return ESS_BREAK;
}
"continue" {
    yylval = yytext;
    return ESS_CONTINUE;
}
"return" {
    yylval = yytext;
    return ESS_RETURN;
}
"local" {
    yylval = yytext;
    return ESS_LOCAL;
}
"function" {
    yylval = yytext;
    return ESS_FUNCTION;
}

\'[^\']*\' {
    yylval = yytext;
    return ESS_STRING;
}
[0-9]+ {
    yylval = yytext;
    return ESS_INT;
}
[_a-zA-Z][_a-zA-Z0-9]* {
    yylval = yytext;
    return ESS_ID;
}

[\+\-\*\/\%]?\= {
    yylval = yytext;
    return ESS_ASSIGN_OP;
}
"&&"|"||" {
    yylval = yytext;
    return ESS_AND_OP;
}
\<|\>|"!="|"=="|"<="|">=" {
    yylval = yytext;
    return ESS_CMP_OP;
}
[\+\-] {
    yylval = yytext;
    return ESS_ADD_OP;
}
[\*\/\%] {
    yylval = yytext;
    return ESS_MUL_OP;
}
"++"|"--" {
    yylval = yytext;
    return ESS_INC_OP;
}
\! {
    yylval = yytext;
    return ESS_UNARY_OP;
}

. {
    yylval = yytext[0];
    return yytext[0];
}

%%

