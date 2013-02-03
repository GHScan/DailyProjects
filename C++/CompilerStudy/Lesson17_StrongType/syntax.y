%{

#ifdef _MSC_VER
#pragma warning(disable : 4065)
#endif

#include "AST.h"
#include "Any.h"
#include "SymbolTable.h"
#include "TypeSystem.h"
#define YYSTYPE Any
#include "lex.yy.c_"

typedef pair<IType*, string> DeclarePair;
typedef vector<DeclarePair> DeclarePairVec;

%}

%token SWITCH IF FOR WHILE DO BREAK CONTINUE RETURN STRUCT CASE DEFAULT
%token TRUE FALSE
%token SIZEOF
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

Struct : STRUCT ID '{' Fields '}' ';' {
            auto vec = $4.get<shared_ptr<DeclarePairVec> >();
            StructType *type = new StructType();
            SymbolTablePtr table(new SymbolTable(NULL));
            for (int i = vec->size() - 1; i >= 0; --i) {
                auto _type = (*vec)[i].first;
                auto _fieldName = (*vec)[i].second;
                type->fields.push_back(_type);
                table->addSymbol(_fieldName, _type);
            }
            TypeSystem::instance()->addType($2.get<string>(), type);
            SymbolTableManager::instance()->addTypeTable(type, table);
       }
       ;

Fields: Fields Field {
            auto p = $1.get<shared_ptr<DeclarePairVec> >();
            $$ = p;
            p->push_back($2.get<DeclarePair>());
      }
      | Field {
            auto p = shared_ptr<DeclarePairVec>(new DeclarePairVec());
            $$ = p;
            p->push_back($1.get<DeclarePair>());
        }
      ;
Field: Declare ';'
     ;

Func : SingleType ID '(' Opt_DeclareList ')' '{' Opt_Stmts '}'  {
            auto args = $4.get<shared_ptr<DeclarePairVec> >();
         }

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
     | SIZEOF '(' Exp ')'
     | SIZEOF '(' Type ')'
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
                | {
                $$ = shared_ptr<DeclarePairVec>(new DeclarePairVec());
                }
                ;
DeclareList : Declare {
                auto p = shared_ptr<DeclarePairVec>(new DeclarePairVec());
                $$ = p;
                p->push_back($1.get<DeclarePair>());
            }
            | DeclareList ',' Declare {
                auto p = $1.get<shared_ptr<DeclarePairVec> >();
                $$ = p;
                p->push_back($3.get<DeclarePair>());
            }
            ;
Declare : Type ID {
        $$ = DeclarePair($1.get<IType*>(), $2.get<string>());
        }
        ;

Type : SingleType 
     | ArrayType
     ;
ArrayType : ArrayType '[' INT ']' {
          $$ = TypeSystem::instance()->getArray($1.get<IType*>(), atoi($3.get<string>().c_str()));
          }
      | SingleType '[' INT ']' {
            $$ = TypeSystem::instance()->getArray($1.get<IType*>(), atoi($3.get<string>().c_str()));
        }
      ;
SingleType: DecorateUserdefineType
          | DecorateBuildinType
          ;
DecorateUserdefineType: UserdefineType
                      | UserdefineType STAR_OP {
                    $$ = TypeSystem::instance()->getPointer($1.get<IType*>());
                }
                      ;
UserdefineType : T_ADT {
               $$ = TypeSystem::instance()->getType($1.get<string>());
               }
           ;
DecorateBuildinType : BuildinType
                    | BuildinType STAR_OP {
                    $$ = TypeSystem::instance()->getPointer($1.get<IType*>());
                    }
                    ;
BuildinType : T_CHAR {
            $$ = TypeSystem::instance()->getType("char");
            }
            | T_INT {
            $$ = TypeSystem::instance()->getType("int");
        }
            | T_VOID {
            $$ = TypeSystem::instance()->getType("void");
        }
            ;
Constants: LITERAL {
             $$ = ExpNodePtr(new ExpNode_ConstantString($1.get<string>()));
         }
         | INT {
            $$ = new ExpNodePtr(new ExpNode_ConstantInt(
                atoi($1.get<string>().c_str())));
        }
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

