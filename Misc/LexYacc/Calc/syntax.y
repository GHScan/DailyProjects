%left AND OR
%left ADD SUB
%left MUL DIV MOD
%nonassoc LP RP
%nonassoc INT

%{
#include "lex.yy.c"
void yyerror(const char *msg);
%}
%%
Calc : Exp { printf("=%d", $1); }
     ;
Exp : Term
    | Exp AND Exp { $$ = $1 && $3; }
    | Exp OR Exp { $$ = $1 || $3; }
    | Exp ADD Exp { $$ = $1 + $3; }
    | Exp SUB Exp { $$ = $1 - $3; }
    | Exp MUL Exp { $$ = $1 * $3; }
    | Exp DIV Exp { $$ = $1 / $3; }
    | Exp MOD Exp { $$ = $1 % $3; }
    ;
Term : INT
     | LP Exp RP { $$ = $2; }
     ;
%%
void yyerror(const char *msg)
{
    printf("error : %s\n", msg);
}
int yywrap()
{
    return 1;
}
int main()
{
    yyparse();
    return 0;
}
