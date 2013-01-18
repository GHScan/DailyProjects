%{
#include "Runtime.h"
#include "AST.h"
#include "lex.yy.c"

#ifdef _MSC_VER
#pragma warning(disable : 4065)
#endif

struct StmtNode_Container:
    public IStmtNode
{
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {}

    vector<StmtNodePtr> toStmtList() const { 
        vector<StmtNodePtr> r;
        for (auto &stmt : stmts) {
            if (stmt == NULL) continue;
            if (auto p = dynamic_cast<StmtNode_Container*>(stmt.get())) {
                auto v = p->toStmtList();
                r.insert(r.end(), v.begin(), v.end());
            }
            else r.push_back(stmt);
        }
        return r; 
    }
};
struct ExpNode_Container:
    public IExpNode
{
    vector<ExpNodePtr> exps;
    virtual void acceptVisitor(IExpNodeVisitor *v) {}

    vector<ExpNodePtr> toExpList() const
    {
        vector<ExpNodePtr> r;
        for (auto &exp : exps) {
            if (exp != NULL) r.push_back(exp);
        }
        return r; 
    }
};

%}

%token IF FOR WHILE DO BREAK CONTINUE RETURN LOCAL FUNCTION
%token INT STRING ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%right ASSIGN_OP
%left AND_OP 
%left CMP_OP
%left ADD_OP
%left MUL_OP
%nonassoc UNARY_OP
%nonassoc INC_OP

%%

Program : Funcs
        ;
Funcs : Funcs Func
      | Func
      ;
Func : FUNCTION ID '(' Opt_IDTuple ')' '{' Opt_Stmts '}' {
        auto func = new ASTFunction();
        GlobalEnvironment::instance()->registerFunc($2.term, FunctionPtr(func));
        auto block = new StmtNode_Block();
        func->stmt.reset(block);
        for (auto &exp: static_cast<ExpNode_Container*>($4.exp.get())->toExpList()) {
            auto v = static_cast<ExpNode_Variable*>(exp.get());
            block->stmts.push_back(StmtNodePtr(new StmtNode_Local(v->name)));
            ++func->argc;
        }
        auto v = static_cast<StmtNode_Container*>($7.stmt.get())->toStmtList();
        block->stmts.insert(block->stmts.end(), v.begin(), v.end());
     }
     ;

Opt_Stmts: Stmts
         | { $$ = new StmtNode_Container(); }
         ;
Stmts : Stmts Stmt {
        $$ = $1; 
        static_cast<StmtNode_Container*>($1.stmt.get())->stmts.push_back($2.stmt);
      }
      | Stmt {
        auto p = new StmtNode_Container();
        $$ = p;
        p->stmts.push_back($1.stmt);
      }
      ;

Stmt: IF '(' Exp ')' Stmt %prec LOWER_THAN_ELSE {
        $$ = new StmtNode_IfElse($3.exp, $5.stmt, StmtNodePtr());
    }
    | IF '(' Exp ')' Stmt ELSE Stmt {
        $$ = new StmtNode_IfElse($3.exp, $5.stmt, $7.stmt);
    }
    | FOR '(' Stmt Opt_Exp ';' Opt_Exp ')' Stmt {
        auto l = dynamic_cast<StmtNode_Exp*>($3.stmt.get());
        if (l == NULL) {
            auto p = new StmtNode_Block();
            $$ = p;
            if ($3.stmt) {
                if (auto c = dynamic_cast<StmtNode_Container*>($3.stmt.get())) {
                    vector<StmtNodePtr> v = c->toStmtList();
                    p->stmts.insert(p->stmts.end(), v.begin(), v.end());
                }
                else {
                    p->stmts.push_back($3.stmt);
                }
            }
            p->stmts.push_back(StmtNodePtr(
                new StmtNode_For(ExpNodePtr(), $4.exp, $6.exp, $8.stmt)));
        }
        else {
            $$ = new StmtNode_For(l->exp, $4.exp, $6.exp, $8.stmt);
        }
    }
    | WHILE '(' Exp ')' Stmt {
        $$ = new StmtNode_For(ExpNodePtr(), $3.exp, ExpNodePtr(), $5.stmt);
    }
    | DO Stmt WHILE '(' Exp ')' ';' {
        auto p = new StmtNode_Container();
        $$ = p;
        p->stmts.push_back($2.stmt);
        p->stmts.push_back(StmtNodePtr(
            new StmtNode_For(ExpNodePtr(), $5.exp, ExpNodePtr(), $2.stmt)));
    }
    | LOCAL ID ';' {
        $$ = new StmtNode_Local($2.term);
    }
    | LOCAL Assign ';' {
        auto p = new StmtNode_Container();
        $$ = p;
        p->stmts.push_back(StmtNodePtr(
            new StmtNode_Local(static_cast<ExpNode_Assign*>($2.exp.get())->name)));
        p->stmts.push_back(StmtNodePtr(new StmtNode_Exp($2.exp)));
    }
    | '{' Opt_Stmts '}' { 
        auto v = static_cast<StmtNode_Container*>($2.stmt.get())->toStmtList();
        if (v.empty()) $$ = SymbolAttribute();
        else $$ = new StmtNode_Block(v);
    }
    | Exp ';' { 
        $$ = new StmtNode_Exp($1.exp); 
    }
    | ';' { $$ = SymbolAttribute(); }
    | CONTINUE ';' {
        $$ = new StmtNode_Continue();
    }
    | BREAK ';' {
        $$ = new StmtNode_Break();
    }
    | RETURN Opt_Exp ';' {
        $$ = new StmtNode_Return($2.exp);
    }
    ;

Call : ID '(' Opt_ExpTuple ')' {
        $$ = new ExpNode_Call($1.term, static_cast<ExpNode_Container*>($3.exp.get())->toExpList());
     }
     ;

Opt_IDTuple: IDTuple
           | { $$ = new ExpNode_Container(); }
           ;
IDTuple : IDTuple ',' ID {
            $$ = $1;
            static_cast<ExpNode_Container*>($1.exp.get())->exps.push_back(
                ExpNodePtr(new ExpNode_Variable($3.term)));
        }
        | ID {
            auto p = new ExpNode_Container();
            $$ = p;
            p->exps.push_back(ExpNodePtr(new ExpNode_Variable($1.term)));
        }
        ;

Opt_ExpTuple: ExpTuple
            | { $$ = new ExpNode_Container(); }
            ;
ExpTuple : ExpTuple ',' Exp {
            $$ = $1;
            static_cast<ExpNode_Container*>($1.exp.get())->exps.push_back($3.exp);
         }
         | Exp {
            auto p = new ExpNode_Container();
            $$ = p;
            p->exps.push_back($1.exp);
         }
         ;

Opt_Exp: Exp
       | { $$ = SymbolAttribute(); }
       ;
Exp : Assign
    | BinExp
    ;

Assign : RAssign BinExp {
        static_cast<ExpNode_Assign*>($1.exp.get())->getRightMost()->right = 
            static_pointer_cast<IExpNode>($2.exp);
       }
       ;
RAssign : RAssign ID ASSIGN_OP {
            // TODO
            ASSERT($3.term == "=");
            static_cast<ExpNode_Assign*>($1.exp.get())->getRightMost()->right.reset(
                new ExpNode_Assign($2.term, ExpNodePtr()));
        }
        | ID ASSIGN_OP { $$ = new ExpNode_Assign($1.term, ExpNodePtr()); }
        ; 

BinExp : Term 
       | BinExp AND_OP BinExp {
        $$ = new ExpNode_BinaryOp($2.term, $1.exp, $3.exp);
       }
       | BinExp ADD_OP BinExp {
        $$ = new ExpNode_BinaryOp($2.term, $1.exp, $3.exp);
       }
       | BinExp MUL_OP BinExp {
        $$ = new ExpNode_BinaryOp($2.term, $1.exp, $3.exp);
       }
       | BinExp CMP_OP BinExp {
        $$ = new ExpNode_BinaryOp($2.term, $1.exp, $3.exp);
       }
       ;

Term : STRING { $$ = new ExpNode_Constant(Value::createString($1.term)); }
     | INT { $$ = new ExpNode_Constant(Value::createInt(atoi($1.term.c_str()))); }
     | ID { $$ = new ExpNode_Variable($1.term); }
     | Call 
     | '(' Exp ')' { $$ = $2; }
     | INC_OP ID {
        if ($1.term == "++" || $1.term == "--") {
            $$ = new ExpNode_Assign($2.term, 
                ExpNodePtr(new ExpNode_BinaryOp($1.term.substr(1), 
                    ExpNodePtr(new ExpNode_Variable($2.term)), 
                    ExpNodePtr(new ExpNode_Constant(Value::createInt(1))))));
        }
        else ASSERT(0);
     }
     | UNARY_OP Term {
        $$ = new ExpNode_UnaryOp($1.term, $2.exp);
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

