
%{
#include "syntax.tab.h"
%}
%%
"&&" return AND;
"||" return OR;
\+ return ADD;
\- return SUB;
\* return MUL;
\/ return DIV;
\% return MOD;
\( return LP;
\) return RP;
[0-9]+ { yylval = atoi(yytext); return INT; }
%%
