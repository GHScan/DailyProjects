%{
#define YY_NO_UNISTD_H

#include "syntax.yy.h"

int isatty(int n);

#ifdef _MSC_VER
#pragma warning(disable : 4267)
#pragma warning(disable : 4244)
#endif

%}

%%

"//"[^\n]*\n {
    yyset_lineno(yyget_lineno() + 1);
    //printf("comments : %s\n", yytext);
}
"/*"([^\*]|\*[^/])*"*/" {
    int lineN = 0;
    for (int i = 0; i < yyleng; ++i) lineN += yytext[i] == '\n';
    yyset_lineno(yyget_lineno() + lineN);

    //printf("comments : %s\n", yytext);
}

\s {
    if (yytext[0] == '\n') {
        yyset_lineno(yyget_lineno() + 1);
    }
}

"if" {
    return ESS_IF;
}
"else" {
    return ESS_ELSE;
}
"for" {
    return ESS_FOR;
}
"while" {
    return ESS_WHILE;
}
"do" { 
    return ESS_DO;
}
"break" {
    return ESS_BREAK;
}
"continue" {
    return ESS_CONTINUE;
}
"return" {
    return ESS_RETURN;
}
"local" {
    return ESS_LOCAL;
}
"function" {
    return ESS_FUNCTION;
}

\'[^\']*\' {
    string s(yytext + 1, yytext + yyleng - 1);
    yylval.term.clear();
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[i + 1]) {
                case 't':
                    ++i;
                    yylval.term.push_back('\t');
                    continue;
                case 'n':
                    ++i;
                    yylval.term.push_back('\n');
                    continue;
                default:
                    break;
            }
        }
        yylval.term.push_back(s[i]);
    }
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
    return yytext[0];
}

%%
