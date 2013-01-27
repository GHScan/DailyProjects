%token BIN_OP
%token INT

%{
#include "lex.yy.c"
void yyerror(const char *msg);
%}
%%
Program : Exp;
Exp : BIN_OP {printf("(");} Exp {printf(")%c(", $1);} Exp {printf(")");}
    | INT { printf("%d ", $1); }
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
