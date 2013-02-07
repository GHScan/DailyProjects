%{

#ifdef _MSC_VER
#pragma warning(disable : 4065)
#endif

#include "AST.h"
#include "Any.h"
#include "SymbolTable.h"
#include "TypeSystem.h"
#include "Runtime.h"
#define YYSTYPE Any
#include "lex.yy.c_"

typedef pair<IType*, string> DeclarePair;
typedef vector<DeclarePair> DeclarePairVec;
typedef vector<ExpNodePtr> ExpNodePtrVec;
typedef pair<string, ExpNodePtr> AssignPair;
typedef vector<AssignPair> AssignPairVec;
typedef shared_ptr<AssignPairVec> AssignPairVecPtr;
typedef pair<IType*, AssignPairVecPtr> TypeAssignPairVecPtr;
typedef pair<ExpNodePtr, StmtNodePtr> ExpStmtPair;
typedef vector<ExpStmtPair> ExpStmtPairVec;
typedef shared_ptr<ExpStmtPairVec> ExpStmtPairVecPtr;

struct StmtNode_Container:
    public IStmtNode
{
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) 
    {
        for (auto stmt : stmts) stmt->acceptVisitor(v);
    }
};
static void insertIntoBlock(const StmtNodePtr &node, StmtNode_Block* block)
{
    if (node == NULL) return;
    if (auto c = dynamic_cast<StmtNode_Container*>(node.get())) {
        for (auto p : c->stmts) insertIntoBlock(p, block);
    }
    else block->stmts.push_back(node);
}

string parseSourceLiteral(const string& s)
{
    string r;
    for (int i = 1; i < s.size() - 1; ++i) {
        if (s[i] == '\\') {
            switch (s[i + 1]) {
                case 't': r.push_back('\t'); break;
                case 'a': r.push_back('\a'); break;
                case 'n': r.push_back('\n'); break;
                case 'r': r.push_back('\r'); break;
                case '\\':r.push_back('\\'); break;
                default: ASSERT(0); break;
            }
            ++i;
        }
        else r.push_back(s[i]);
    }
    return r;
}

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
        | Define {
            auto typeVec = $1.get<TypeAssignPairVecPtr>();
            auto type = typeVec.first;
            auto vec = typeVec.second;
            auto block = dynamic_cast<StmtNode_Block*>(CodeManager::instance()->getFuncPreMain()->getStmt().get());
            auto table = SymbolTableManager::instance()->global();
            for (auto nameExp : *vec) {
                table->addSymbol(nameExp.first, type);
            }
            for (auto nameExp : *vec) {
                block->stmts.push_back(StmtNodePtr(new StmtNode_Exp(ExpNodePtr(
                    new ExpNode_Assign(ExpNodePtr(new ExpNode_Variable(nameExp.first)), nameExp.second)))));
            }
        }
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
            auto args = $4.get<shared_ptr<DeclarePairVec> >();

            auto block = new StmtNode_Block();
            StmtNodePtr pblock(block);
            block->stmts.push_back(StmtNodePtr(new StmtNode_DefineLocal("return", $1.get<IType*>())));
            for (auto arg : *args) {
                block->stmts.push_back(StmtNodePtr(new StmtNode_DefineLocal(arg.second, arg.first)));
            }
            insertIntoBlock($7.get<StmtNodePtr>(), block);

            IType *type = NULL;
            {
                vector<IType*> argsT;
                for (auto arg : *args) argsT.push_back(arg.first);
                type = TypeSystem::instance()->getFunc($1.get<IType*>(), argsT);
            }
            CodeManager::instance()->registerFunction($2.get<string>(), FunctionPtr(new ASTFunction(pblock)));

            auto table = SymbolTableManager::instance()->global();
            table->addSymbol($2.get<string>(), type);
         }

Opt_Stmts: Stmts
         | {
        $$ = StmtNodePtr(new StmtNode_Container());
        }
         ;
Stmts : Stmts Stmt {
        auto c = dynamic_cast<StmtNode_Container*>($1.get<StmtNodePtr>().get());
        c->stmts.push_back($2.get<StmtNodePtr>());
        $$ = $1;
      }
      |  Stmt {
        auto c = new StmtNode_Container();
        c->stmts.push_back($1.get<StmtNodePtr>());
        $$ = StmtNodePtr(c);
        }
      ;
Stmt : Define {
        auto c = new StmtNode_Container();
        StmtNodePtr p(c);
        auto typeVec = $1.get<TypeAssignPairVecPtr>();
        auto type = typeVec.first;
        auto vec = typeVec.second;
        for (auto nameExp : *vec) {
            c->stmts.push_back(StmtNodePtr(new StmtNode_DefineLocal(nameExp.first, type)));
        }
        for (auto nameExp : *vec) {
            if (nameExp.second) {
                c->stmts.push_back(StmtNodePtr(new StmtNode_Exp(ExpNodePtr(
                    new ExpNode_Assign(ExpNodePtr(new ExpNode_Variable(nameExp.first)), nameExp.second)))));
            }
        }
        if (c->stmts.empty()) $$ = StmtNodePtr();
        else $$ = p;
     }
     | Switch
     | '{' Opt_Stmts '}' {
        auto block = new StmtNode_Block();
        StmtNodePtr p(block);
        insertIntoBlock($2.get<StmtNodePtr>(), block);
        if (block->stmts.empty()) $$ = StmtNodePtr();
        else $$ = p;
    }
     | Opt_Exp ';' {
        auto exp = $1.get<ExpNodePtr>();
        if (exp) $$ = StmtNodePtr(new StmtNode_Exp(exp));
        else $$ = StmtNodePtr();
    }
     | BREAK ';' {
        $$ = StmtNodePtr(new StmtNode_Break());
    }
     | CONTINUE ';' {
        $$ = StmtNodePtr(new StmtNode_Continue());
    }
     | RETURN Opt_Exp ';' {
        auto exp = $2.get<ExpNodePtr>();
        if (exp) {
            auto c = new StmtNode_Container();
            c->stmts.push_back(StmtNodePtr(new StmtNode_Exp(ExpNodePtr(
                new ExpNode_Assign(ExpNodePtr(new ExpNode_Variable("return")), exp)))));
            c->stmts.push_back(StmtNodePtr(new StmtNode_Return()));
            $$ = StmtNodePtr(c);
        }
        else $$ = StmtNodePtr(new StmtNode_Return());
    }
     | IF '(' Exp ')' Stmt ELSE Stmt {
        $$ = StmtNodePtr(new StmtNode_IfElse(
            $3.get<ExpNodePtr>(), $5.get<StmtNodePtr>(), $7.get<StmtNodePtr>()));
    }
     | IF '(' Exp ')' Stmt %prec LOWER_THAN_ELSE {
        $$ = StmtNodePtr(new StmtNode_IfElse(
            $3.get<ExpNodePtr>(), $5.get<StmtNodePtr>(), StmtNodePtr()));
    }
     | WHILE '(' Exp ')' Stmt {
        $$ = StmtNodePtr(new StmtNode_For(
            ExpNodePtr(), $3.get<ExpNodePtr>(), ExpNodePtr(), $5.get<StmtNodePtr>()));
    }
     | DO Stmt WHILE '(' Exp ')' ';' {
        auto c = new StmtNode_Container();
        c->stmts.push_back($2.get<StmtNodePtr>());
        c->stmts.push_back(StmtNodePtr(new StmtNode_For(
            ExpNodePtr(), $5.get<ExpNodePtr>(), ExpNodePtr(), $2.get<StmtNodePtr>())));
        $$ = StmtNodePtr(c);
    }
     | FOR '(' Stmt Opt_Exp';' Opt_Exp ')' Stmt {
        auto stmt1 = $3.get<StmtNodePtr>();
        if (auto define = dynamic_cast<StmtNode_Container*>(stmt1.get())) {
            auto block = new StmtNode_Block();
            insertIntoBlock(stmt1, block);
            block->stmts.push_back(StmtNodePtr(new StmtNode_For(
                ExpNodePtr(), $4.get<ExpNodePtr>(), $6.get<ExpNodePtr>(), $8.get<StmtNodePtr>())));
            $$ = StmtNodePtr(block);
        }
        else {
            auto exp1 = ExpNodePtr();
            if (auto stmtExp = dynamic_cast<StmtNode_Exp*>(stmt1.get())) {
                exp1 = stmtExp->exp;
            }
            else ASSERT(stmt1 == NULL);
            $$ = StmtNodePtr(new StmtNode_For(
                exp1, $4.get<ExpNodePtr>(), $6.get<ExpNodePtr>(), $8.get<StmtNodePtr>()));
        }
     }
    ;

Define : Type InitVars ';' {
       $$ = TypeAssignPairVecPtr($1.get<IType*>(), $2.get<AssignPairVecPtr>());
       }
       ;
InitVars : InitVars ',' InitVal {
             auto p = $1.get<AssignPairVecPtr>();
             $$ = p;
             p->push_back($3.get<AssignPair>());
         }
         | InitVal {
             auto p = AssignPairVecPtr(new AssignPairVec());
             $$ = p;
             p->push_back($1.get<AssignPair>());
         }
         ;
InitVal : ID {
        $$ = AssignPair($1.get<string>(), ExpNodePtr());
        }
        | ID ASSIGN_OP Assign {
        auto r = $3.get<ExpNodePtr>();
        auto op = $2.get<string>();
        if (op != "=") {
            r = ExpNodePtr(new ExpNode_BinaryOp(
                    ExpNodePtr(new ExpNode_Variable($1.get<string>())), r,
                    ExpNode_BinaryOp::string2op(op.substr(0, 1))));
        }
        $$ = AssignPair($1.get<string>(), r);
        }
        ;

Switch : SWITCH '(' Exp ')' '{' CaseOrDefaultList '}' {
            auto stmt = new StmtNode_Switch($3.get<ExpNodePtr>());
            StmtNodePtr lastBlock;
            auto vec = $6.get<ExpStmtPairVecPtr>();
            reverse(vec->begin(), vec->end());
            for (auto p : *vec) {
                auto block = new StmtNode_Block();
                StmtNodePtr pblock(block);
                insertIntoBlock(p.second, block);
                if (block->stmts.empty()) {
                    if (lastBlock == NULL) continue;
                }
                else lastBlock = pblock;

                if (auto expInt = dynamic_cast<ExpNode_ConstantInt*>(p.first.get())) {
                    stmt->caseMap[expInt->value] = lastBlock;
                }
                else if (auto expLiteral = dynamic_cast<ExpNode_ConstantLiteral*>(p.first.get())) {
                    ASSERT(0);
                }
                else {
                    ASSERT(stmt->defaultStmt == NULL);
                    stmt->defaultStmt = lastBlock;
                }
            }
            ASSERT(!stmt->caseMap.empty());
            $$ = StmtNodePtr(stmt);
       }
       ;
CaseOrDefaultList: CaseOrDefaultList CaseOrDefault {
                    auto p = $1.get<ExpStmtPairVecPtr>();
                    p->push_back($2.get<ExpStmtPair>());
                    $$ = p;
                 }
                 | CaseOrDefault {
                auto p = ExpStmtPairVecPtr(new ExpStmtPairVec());
                p->push_back($1.get<ExpStmtPair>());
                $$ = p;
                }
                 ;
CaseOrDefault: Case
             | Default
             ;
Case : CASE Constants ':' Opt_Stmts {
        $$ = ExpStmtPair($2.get<ExpNodePtr>(), $4.get<StmtNodePtr>());
     }
     ;
Default: DEFAULT ':' Opt_Stmts {
        $$ = ExpStmtPair(
            ExpNodePtr(new ExpNode_Variable("default")),
            $3.get<StmtNodePtr>());
       }
     ;

Call: ID '(' Opt_ExpList ')' {
    $$ = ExpNodePtr(new ExpNode_Call($1.get<string>(), *$3.get<shared_ptr<ExpNodePtrVec> >()));
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
     $$ = ExpNodePtr(new ExpNode_Sizeof($3.get<ExpNodePtr>()));
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
     $$ = ExpNodePtr(new ExpNode_ConstantLiteral(parseSourceLiteral(s)));
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
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>(), false));
     }
     | Term AFIELD_OP ID {
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>(), true));
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
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>(), false));
     }
     | Term AFIELD_OP ID {
     $$ = ExpNodePtr(new ExpNode_Field($1.get<ExpNodePtr>(), $3.get<string>(), true));
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
             auto s = $1.get<string>();
             $$ = ExpNodePtr(new ExpNode_ConstantLiteral(parseSourceLiteral(s)));
         }
         | INT {
            $$ = ExpNodePtr(new ExpNode_ConstantInt(
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

