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
typedef vector<ExpNodePtr> ExpNodePtrVec;

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
        | Define
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

Func : Type ID '(' Opt_DeclareList ')' '{' Opt_Stmts '}'  {
            // FIXME: Here should be SingleType not Type...
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

Define : Type InitVars ';'
       ;
InitVars : InitVars ',' InitVal
         | InitVal
         ;
InitVal : ID
        | ID ASSIGN_OP Assign
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

Call: ID '(' Opt_ExpList ')' {
    $$ = ExpNodePtr(new ExpNode_Call(
        $1.get<string>(), *$3.get<shared_ptr<ExpNodePtrVec> >()));
    }
    ; 
Opt_ExpList : ExpList
         | {
             $$ = shared_ptr<ExpNodePtrVec>(new ExpNodePtrVec());
         }
         ;
ExpList : ExpList ',' Exp {
         auto p = $1.get<shared_ptr<ExpNodePtrVec> >();
         $$ = p;
         p->push_back($3.get<ExpNodePtr>());
        }
     | Exp {
     auto p = shared_ptr<ExpNodePtrVec>(new ExpNodePtrVec());
     $$ = p;
     p->push_back($1.get<ExpNodePtr>());
     }
     ;

Opt_Exp: Exp
       | {
       $$ = ExpNodePtr();
       }
       ;
Exp : Assign
    ;
Assign : LVal ASSIGN_OP Assign {
       auto r = $3.get<ExpNodePtr>();
       auto op = $2.get<string>();
       if (op != "=") {
           r = ExpNodePtr(new ExpNode_BinaryOp($1.get<ExpNodePtr>(), r, ExpNode_BinaryOp::string2op(op.substr(0, 1))));
       }
       $$ = ExpNodePtr(new ExpNode_Assign($1.get<ExpNodePtr>(), r));
       }
       | Log
       ;
Log : Log LOG_OP Rel {
    $$ = ExpNodePtr(new ExpNode_BinaryOp(
        $1.get<ExpNodePtr>(), 
        $3.get<ExpNodePtr>(),
        ExpNode_BinaryOp::string2op($2.get<string>())));
    }
    | Rel
    ;
Rel : Rel REL_OP Add {
    $$ = ExpNodePtr(new ExpNode_BinaryOp(
        $1.get<ExpNodePtr>(), 
        $3.get<ExpNodePtr>(),
        ExpNode_BinaryOp::string2op($2.get<string>())));
    }
    | Add
    ;
Add : Add ADD_OP Mul {
    $$ = ExpNodePtr(new ExpNode_BinaryOp(
        $1.get<ExpNodePtr>(), 
        $3.get<ExpNodePtr>(),
        ExpNode_BinaryOp::string2op($2.get<string>())));
    }
    | Mul
    ;
Mul : Mul STAR_OP Term {
    $$ = ExpNodePtr(new ExpNode_BinaryOp($1.get<ExpNodePtr>(), $3.get<ExpNodePtr>(), ExpNode_BinaryOp::BO_Mul));
    }
    | Mul DIV_OP Term {
    $$ = ExpNodePtr(new ExpNode_BinaryOp($1.get<ExpNodePtr>(), $3.get<ExpNodePtr>(), ExpNode_BinaryOp::BO_Div));
    }
    | Mul MOD_OP Term  {
    $$ = ExpNodePtr(new ExpNode_BinaryOp($1.get<ExpNodePtr>(), $3.get<ExpNodePtr>(), ExpNode_BinaryOp::BO_Mod));
    }
    | Term
    ;
Term : '(' Exp ')' {
     $$ = $2;
     }
     | SIZEOF '(' Exp ')' {
     $$ = ExpNodePtr(new ExpNode_ConstantInt($3.get<ExpNodePtr>()->type->getSize()));
     }
     | SIZEOF '(' Type ')' {
     $$ = ExpNodePtr(new ExpNode_ConstantInt($3.get<IType*>()->getSize()));
     }
     | Call 
     | INC_OP ID {
     auto id = ExpNodePtr(new ExpNode_Variable($2.get<string>()));
     $$ = ExpNodePtr(new ExpNode_UnaryOp(id, ExpNode_UnaryOp::string2op($1.get<string>())));
     }
     | NOT_OP Term {
     $$ = ExpNodePtr(new ExpNode_UnaryOp($2.get<ExpNodePtr>(), ExpNode_UnaryOp::UO_Not));
     }
     | ID {
     $$ = ExpNodePtr(new ExpNode_Variable($1.get<string>()));
     }
     | INT {
     $$ = ExpNodePtr(new ExpNode_ConstantInt(atoi($1.get<string>().c_str())));
     }
     | LITERAL {
     auto s = $1.get<string>();
     $$ = ExpNodePtr(new ExpNode_ConstantString(s.substr(1, s.size() - 2)));
     }
     | TRUE {
     $$ = ExpNodePtr(new ExpNode_ConstantInt(1));
     }
     | FALSE {
     $$ = ExpNodePtr(new ExpNode_ConstantInt(0));
     }
     | ArrayAccess
     | STAR_OP Term %prec NOT_OP {
     $$ = ExpNodePtr(new ExpNode_Unref($2.get<ExpNodePtr>()));
     }
     | '&' Term %prec NOT_OP {
     $$ = ExpNodePtr(new ExpNode_Addr($2.get<ExpNodePtr>()));
     }
     | '(' SingleType ')' Term %prec NOT_OP {
     $$ = ExpNodePtr(new ExpNode_Conversion($4.get<ExpNodePtr>(), $2.get<IType*>()));
     }
     | Term POINTER_AFIELD_OP ID {
     // TODO: assert pointer
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>()));
     }
     | Term AFIELD_OP ID {
     // TODO: assert not pointer
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>()));
     }
     ;

LVal : ID {
     $$ = ExpNodePtr(new ExpNode_Variable($1.get<string>()));
     }
     | ArrayAccess
     | STAR_OP Term {
     $$ = ExpNodePtr(new ExpNode_Unref($2.get<ExpNodePtr>()));
     }
     | Term POINTER_AFIELD_OP ID {
     // TODO: assert pointer
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>()));
     }
     | Term AFIELD_OP ID {
     // TODO: assert not pointer
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>()));
     }
     ;

ArrayAccess : ArrayAccess '[' Exp ']' {
            $$ = ExpNodePtr(new ExpNode_ArrayElem(
                $1.get<ExpNodePtr>(),
                $3.get<ExpNodePtr>()));
            }
           | ID '[' Exp ']' {
           $$ = ExpNodePtr(new ExpNode_ArrayElem(
                ExpNodePtr(new ExpNode_Variable($1.get<string>())),
                $3.get<ExpNodePtr>()));
           }
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
        yyerror("Parsing exception!");
        fclose(f);
        throw;
    }
}

