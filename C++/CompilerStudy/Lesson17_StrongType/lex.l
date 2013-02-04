%{
#define YY_NO_UNISTD_H

#include "syntax.yy.h_"

int isatty(int n);
void onTokenFound();

#ifdef _MSC_VER
#pragma warning(disable : 4267)
#pragma warning(disable : 4244)
#endif

%}

%%

"//".*\n {
    onTokenFound();
    //printf("comments : %s\n", yytext);
}
"/*"([^\*]|\*[^/])*"*/" {
    onTokenFound();
    //printf("comments : %s\n", yytext);
}

[ \n\t]+ {
    onTokenFound();
}

"switch" {
    onTokenFound();
    return SWITCH;
}
"case" {
    onTokenFound();
    return CASE;
}
"default" {
    onTokenFound();
    return DEFAULT;
}
"if" {
    onTokenFound();
    return IF;
}
"else" {
    onTokenFound();
    return ELSE;
}
"for" {
    onTokenFound();
    return FOR;
}
"while" {
    onTokenFound();
    return WHILE;
}
"do" { 
    onTokenFound();
    return DO;
}
"break" {
    onTokenFound();
    return BREAK;
}
"continue" {
    onTokenFound();
    return CONTINUE;
}
"return" {
    onTokenFound();
    return RETURN;
}
"true" {
    onTokenFound();
    return TRUE;
}
"false" {
    onTokenFound();
    return FALSE;
}
"int" {
    onTokenFound();
    return T_INT;
}
"char" {
    onTokenFound();
    return T_CHAR;
}
"void" {
    onTokenFound();
    return T_VOID;
}
"struct" {
    onTokenFound();
    return STRUCT;
}
"sizeof" {
    return SIZEOF;
}


\"[^\"]*\" {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    return LITERAL;
}
\-?[0-9]+ {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    return INT;
}
[_a-zA-Z][_a-zA-Z0-9]* {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    if (TypeSystem::instance()->getType(yylval.get<string>())) {
        return T_ADT;
    }
    return ID;
}

[\+\-\*\/\%]?\= {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    return ASSIGN_OP;
}
"&&"|"||" {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    return LOG_OP;
}
\<|\>|"!="|"=="|"<="|">=" {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    return REL_OP;
}
[\+\-] {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    return ADD_OP;
}
\* {
    onTokenFound();
    return STAR_OP;
}
\/ {
    onTokenFound();
    return DIV_OP;
}
\% {
    onTokenFound();
    return MOD_OP;
}
"++"|"--" {
    onTokenFound();
    yylval = string(yytext, yytext + yyleng);
    return INC_OP;
}
\! {
    onTokenFound();
    return NOT_OP;
}
"->" {
    onTokenFound();
    return POINTER_AFIELD_OP;
}
\. {
    return AFIELD_OP;
}

. {
    onTokenFound();
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
int g_column, g_line = 1;
void yyerror(const char *msg)
{
    printf("sytnax error %d,%d : %s\n", g_line, g_column, msg);
}
void onTokenFound()
{
    for (int i = 0; i < yyleng; ++i) {
        if (yytext[i] == '\n') {
            ++g_line;
            g_column = 0;
        }
        else {
            ++g_column;
        }
    }
}
