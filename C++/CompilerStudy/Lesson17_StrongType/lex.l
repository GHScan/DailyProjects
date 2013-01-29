%{
#define YY_NO_UNISTD_H

#include "syntax.yy.h_"

int isatty(int n);

#ifdef _MSC_VER
#pragma warning(disable : 4267)
#pragma warning(disable : 4244)
#endif

%}

%%

"//".*\n {
    yyset_lineno(yyget_lineno() + 1);
    //printf("comments : %s\n", yytext);
}
"/*"([^\*]|\*[^/])*"*/" {
    int lineN = 0;
    for (int i = 0; i < yyleng; ++i) lineN += yytext[i] == '\n';
    yyset_lineno(yyget_lineno() + lineN);

    //printf("comments : %s\n", yytext);
}

[ \t\n] {
    if (yytext[0] == '\n') {
        yyset_lineno(yyget_lineno() + 1);
    }
}

"if" {
    return IF;
}
"else" {
    return ELSE;
}
"for" {
    return FOR;
}
"while" {
    return WHILE;
}
"do" { 
    return DO;
}
"break" {
    return BREAK;
}
"continue" {
    return CONTINUE;
}
"return" {
    return RETURN;
}
"true" {
    return TRUE;
}
"false" {
    return FALSE;
}
"int" {
    return T_INT;
}
"string" {
    return T_STRING;
}
"struct" {
    return STRUCT;
}


\'[^\']*\' {
    return STRING;
}
[0-9]+ {
    return INT;
}
[_a-zA-Z][_a-zA-Z0-9]* {
    return ID;
}

[\+\-\*\/\%]?\= {
    return ASSIGN_OP;
}
"&&"|"||" {
    return LOG_OP;
}
\<|\>|"!="|"=="|"<="|">=" {
    return REL_OP;
}
[\+\-] {
    return ADD_OP;
}
[\*\/\%] {
    return MUL_OP;
}
"++"|"--" {
    return INC_OP;
}
\! {
    return NOT_OP;
}

. {
    return yytext[0];
}

%%

int isatty(int n)
{
    return 0;
}
int yywrap()
{
    return 1;
}
void yyerror(const char *msg)
{
    printf("sytnax error %d : %s\n", yyget_lineno(), msg);
}
