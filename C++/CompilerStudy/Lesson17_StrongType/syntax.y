%{
#include "lex.yy.c_"

#ifdef _MSC_VER
#pragma warning(disable : 4065)
#endif


%}

%token IF FOR WHILE DO BREAK CONTINUE RETURN T_STRING T_INT TRUE FALSE STRUCT
%token INT STRING ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%right ASSIGN_OP
%left LOG_OP 
%left REL_OP
%left ADD_OP
%left MUL_OP
%nonassoc NOT_OP
%nonassoc INC_OP

%%

Program : Program TopMost
        | TopMost
        ;
TopMost : Struct
        | Func
        ;

Fields: Fields Field
      | Field
      ;
Field: Declare ';'
     ;

Struct : STRUCT ID '{' Fields '}' ';'
       ;
Func : Type ID '(' Opt_DeclareList ')' '{' Stmts '}'

Stmts : Stmts Stmt
      |  Stmt
      ;
Stmt : DeclareMul ';'
     | '{' Stmts '}'
     | Opt_Exp ';'
     | BREAK ';'
     | CONTINUE ';'
     | RETURN Opt_Exp ';'
     | IF '(' Exp ')' Stmt ELSE Stmt 
     | IF '(' Exp ')' Stmt %prec LOWER_THAN_ELSE
     | WHILE '(' Exp ')' Stmt
     | DO Stmt WHILE '(' Exp ')' ';'
     | FOR '(' Stmt Opt_Exp';' Opt_Exp ')' Stmt
    ;


ExpList : ExpList ',' Exp
     | Exp
     ;
Opt_ExpList : ExpList
         | 
         ;
Call: ID '(' Opt_ExpList ')'
    ; 

Opt_Exp: Exp
       |
       ;
Exp : Assign
    ;
Assign : LVal ASSIGN_OP Assign
       | Log
       ;
Log : Log LOG_OP Rel
    | Rel
    ;
Rel : Rel REL_OP Add
    | Add
    ;
Add : Add ADD_OP Mul
    | Mul
    ;
Mul : Mul MUL_OP Term
    | Term
    ;
Term : '(' Exp ')'
     | Call
     | INC_OP ID
     | NOT_OP Term
     | ID
     | INT
     | STRING
     | TRUE
     | FALSE
     | ArrayAccess
     | Unref
     ;
LVal : ID
     | ArrayAccess
     | Unref
     ;

Unref : '*' Term
      ;
ArrayAccess : ArrayAccess '[' Exp ']'
           | ID '[' Exp ']'
           ;
 
DeclareMul : DeclareMul_L ID
         ;
DeclareMul_L : Type 
           | DeclareMul_L ID ','
        ;

Opt_DeclareList : DeclareList
                | 
                ;
DeclareList : Declare 
            | DeclareList ',' Declare
            ;
Declare : Type ID
        ;
Type : TermType 
     | TermType '*'
     | Array
     ;
Array : Array '[' INT ']'
      | BuildinType '[' INT ']'
      ;
TermType : BuildinType
     | ID
     ;
BuildinType : T_STRING
            | T_INT
            ;

%%

void parseFile(const char *fname)
{
    FILE *f = fopen(fname, "r");
    ASSERT(f);
    try
    {
        yyrestart(f);
        yyparse();
    } 
    catch (const exception&) {
        fclose(f);
        throw;
    }
}

