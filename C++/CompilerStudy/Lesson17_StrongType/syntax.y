%{

#ifdef _MSC_VER
#pragma warning(disable : 4065)
#endif

#define YYSTYPE string
extern set<string> g_userDefineTypes;
#include "lex.yy.c_"

%}

%token SWITCH IF FOR WHILE DO BREAK CONTINUE RETURN STRUCT CASE DEFAULT
%token TRUE FALSE
%token T_VOID T_INT T_CHAR
%token T_ADT
%token INT LITERAL ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%right ASSIGN_OP
%left LOG_OP 
%left REL_OP
%left ADD_OP
%left STAR_OP DIV_OP MOD_OP
%nonassoc NOT_OP INC_OP
%nonassoc POINTER_AFIELD_OP AFIELD_OP

%%

Program : Program GlobalDefine
        | GlobalDefine
        ;
GlobalDefine : Struct
        | Func
        ;

Fields: Fields Field
      | Field
      ;
Field: Declare ';'
     ;

Struct : STRUCT ID '{' Fields '}' ';' {
       g_userDefineTypes.insert($2);
       }
       ;
Func : SingleType ID '(' Opt_DeclareList ')' '{' Opt_Stmts '}' 

Opt_Stmts: Stmts
         |
         ;
Stmts : Stmts Stmt
      |  Stmt
      ;
Stmt : Define
     | Switch
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

Switch : SWITCH '(' Exp ')' '{' CaseOrDefaultList '}'
       ;
CaseOrDefaultList: CaseOrDefaultList CaseOrDefault
                 | CaseOrDefault
                 ;
CaseOrDefault: Case
             | Default
             ;
Case : CASE Constants ':' Opt_Stmts 
     ;
Default: DEFAULT ':' Opt_Stmts
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
Mul : Mul STAR_OP Term
    | Mul DIV_OP Term
    | Mul MOD_OP Term
    | Term
    ;
Term : '(' Exp ')'
     | Call
     | INC_OP ID
     | NOT_OP Term
     | ID
     | INT
     | LITERAL
     | TRUE
     | FALSE
     | ArrayAccess
     | STAR_OP Term %prec NOT_OP
     | '&' Term %prec NOT_OP
     | '(' SingleType ')' Term %prec NOT_OP
     | Term POINTER_AFIELD_OP ID
     | Term AFIELD_OP ID 
     ;

LVal : ID
     | ArrayAccess
     | STAR_OP Term
     | Term POINTER_AFIELD_OP ID
     | Term AFIELD_OP ID 
     ;

ArrayAccess : ArrayAccess '[' Exp ']'
           | ID '[' Exp ']'
           ;
 
Define : Type InitVarList ';'
       ;
InitVarList : InitVarList ',' InitVar
            | InitVar
            ;
InitVar : ID
        | ID ASSIGN_OP Assign
        ;

Opt_DeclareList : DeclareList
                | 
                ;
DeclareList : Declare 
            | DeclareList ',' Declare
            ;
Declare : Type ID
        ;
Type : SingleType 
     | ArrayType
     ;
ArrayType : ArrayType '[' INT ']'
      | SingleType '[' INT ']'
      ;
SingleType: DecorateUserdefineType
          | DecorateBuildinType
          ;
DecorateUserdefineType: UserdefineType
                      | UserdefineType STAR_OP
                      ;
UserdefineType : T_ADT
           ;
DecorateBuildinType : BuildinType
                    | BuildinType STAR_OP
                    ;
BuildinType : T_CHAR
            | T_INT
            | T_VOID
            ;
Constants: LITERAL
         | INT
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

set<string> g_userDefineTypes;
