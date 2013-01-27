
%{
#include "syntax.tab.h"
%}
%%

[\+\-\*\/\%] {
    yylval = yytext[0];
    return BIN_OP;
}
[0-9]+ { yylval = atoi(yytext); return INT; }
.|\n {}
%%
