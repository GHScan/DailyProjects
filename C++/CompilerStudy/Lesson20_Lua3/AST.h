
#ifndef AST_H
#define AST_H

struct LuaFunctionMeta;
typedef shared_ptr<LuaFunctionMeta> LuaFunctionMetaPtr;

// IExpNode
struct ExpNode_UnaryOp;
struct ExpNode_BinaryOp;
struct ExpNode_Const;
struct ExpNode_LocalVar;
struct ExpNode_UpValueVar;
struct ExpNode_GlobalVar;
struct ExpNode_FieldAccess;
struct ExpNode_TableConstructor;
struct ExpNode_Lambda;
struct ExpNode_Call;
struct ExpNode_Args;
struct IExpNodeVisitor {
    virtual ~IExpNodeVisitor(){}
    virtual void visit(ExpNode_UnaryOp *node) = 0;
    virtual void visit(ExpNode_BinaryOp *node) = 0;
    virtual void visit(ExpNode_Const *node) = 0;
    virtual void visit(ExpNode_LocalVar *node) = 0;
    virtual void visit(ExpNode_UpValueVar *node) = 0;
    virtual void visit(ExpNode_GlobalVar *node) = 0;
    virtual void visit(ExpNode_FieldAccess *node) = 0;
    virtual void visit(ExpNode_TableConstructor *node) = 0;
    virtual void visit(ExpNode_Lambda *node) = 0;
    virtual void visit(ExpNode_Call *node) = 0;
    virtual void visit(ExpNode_Args *node) = 0;
};

struct IExpNode {
    int line;
    IExpNode(int _line): line(_line){}
    virtual void acceptVisitor(IExpNodeVisitor *v) = 0;
};
typedef shared_ptr<IExpNode> ExpNodePtr;

struct ExpNode_UnaryOp:
    public IExpNode {
    enum OpType {
        OP_Not,
        OP_Minus,
        OP_Len,
    };
    ExpNodePtr exp;
    OpType op;
    ExpNode_UnaryOp(OpType _op, const ExpNodePtr& _exp): IExpNode(_exp->line), exp(_exp), op(_op){ }
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_BinaryOp:
    public IExpNode {
    enum OpType {
        OP_And, OP_Or,
        OP_Less, OP_LessEq,
        OP_Greater, OP_GreaterEq,
        OP_Equal, OP_NEqual,
        OP_Add, OP_Sub, OP_Mul, OP_Div, OP_Mod, OP_Pow,
        OP_Concat,
    };
    ExpNodePtr lexp, rexp;
    OpType op;
    ExpNode_BinaryOp(OpType _op, const ExpNodePtr& _lexp, const ExpNodePtr& _rexp):
        IExpNode(_lexp->line), lexp(_lexp), rexp(_rexp), op(_op){}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Const:
    public IExpNode {
    int constIdx;
    ExpNode_Const(int _constIdx, int _line): IExpNode(_line), constIdx(_constIdx) { }
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_LocalVar:
    public IExpNode {
    int localIdx;
    ExpNode_LocalVar(int _localIdx, int line): IExpNode(line), localIdx(_localIdx){}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_UpValueVar:
    public IExpNode {
    int uvIdx;
    ExpNode_UpValueVar(int _uvIdx, int line): IExpNode(line), uvIdx(_uvIdx){}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_GlobalVar:
    public IExpNode {
    int constIdx;
    ExpNode_GlobalVar(int _constIdx, int line): IExpNode(line), constIdx(_constIdx){}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_FieldAccess:
    public IExpNode {
    ExpNodePtr table, field;
    ExpNode_FieldAccess(const ExpNodePtr& _table, const ExpNodePtr& _field): 
        IExpNode(_table->line), table(_table), field(_field) {}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_TableConstructor:
    public IExpNode {
    vector<ExpNodePtr> array;
    vector<pair<ExpNodePtr, ExpNodePtr> > dict;
    ExpNode_TableConstructor(): IExpNode(0){}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Lambda:
    public IExpNode {
    LuaFunctionMetaPtr meta;
    ExpNode_Lambda(const LuaFunctionMetaPtr& _meta);
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Call:
    public IExpNode {
    ExpNodePtr func;
    vector<ExpNodePtr> params;
    ExpNode_Call(const ExpNodePtr& _func, const vector<ExpNodePtr>& _params):
        IExpNode(_func->line), func(_func), params(_params){}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Args:
    public IExpNode {
    ExpNode_Args(int line): IExpNode(line){}
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};


// IStmtNode
struct StmtNode_Call;
struct StmtNode_Assign;
struct StmtNode_Break;
struct StmtNode_Continue;
struct StmtNode_Return;
struct StmtNode_Block;
struct StmtNode_IfElse;
struct StmtNode_RangeFor;
struct StmtNode_LoopFor;
struct StmtNode_IteratorFor;
struct IStmtNodeVisitor {
    virtual ~IStmtNodeVisitor(){}
    virtual void visit(StmtNode_Call *node) = 0;
    virtual void visit(StmtNode_Assign *node) = 0;
    virtual void visit(StmtNode_Break *node) = 0;
    virtual void visit(StmtNode_Continue *node) = 0;
    virtual void visit(StmtNode_Return *node) = 0;
    virtual void visit(StmtNode_Block *node) = 0;
    virtual void visit(StmtNode_IfElse *node) = 0;
    virtual void visit(StmtNode_RangeFor *node) = 0;
    virtual void visit(StmtNode_LoopFor *node) = 0;
    virtual void visit(StmtNode_IteratorFor *node) = 0;
};
struct IStmtNode {
    int line;
    IStmtNode(int _line): line(_line){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct StmtNode_Call:
    public IStmtNode {
    ExpNodePtr callExp;
    StmtNode_Call(const ExpNodePtr& _exp): IStmtNode(_exp->line), callExp(_exp){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Assign:
    public IStmtNode {
    vector<ExpNodePtr> lvalues, rvalues;
    StmtNode_Assign(const vector<ExpNodePtr>& _lvalues, const vector<ExpNodePtr>& _rvalues):
        IStmtNode(_lvalues[0]->line), lvalues(_lvalues), rvalues(_rvalues){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Break:
    public IStmtNode {
    StmtNode_Break(int line): IStmtNode(line){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Continue:
    public IStmtNode {
    StmtNode_Continue(int line): IStmtNode(line){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Return:
    public IStmtNode {
    vector<ExpNodePtr> exps;
    StmtNode_Return(const vector<ExpNodePtr>& _exps): IStmtNode(_exps.empty() ? 0 : _exps[0]->line), exps(_exps){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Block:
    public IStmtNode {
    vector<StmtNodePtr> stmts;
    int localOff, localCount;
    StmtNode_Block(const vector<StmtNodePtr>& _stmts, int _localOff, int _localCount):
        IStmtNode(0), localOff(_localOff), localCount(_localCount){
            for (auto &stmt : _stmts) {
                if (stmt != NULL) stmts.push_back(stmt);
            }
            if (!stmts.empty()) line = stmts[0]->line;
        }
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
typedef pair<ExpNodePtr, StmtNodePtr> ExpStmtPair;
struct StmtNode_IfElse:
    public IStmtNode {
    vector<ExpStmtPair> ifExpStmts;
    StmtNodePtr elseStmt;
    StmtNode_IfElse(const vector<ExpStmtPair>& _ifExpStmts, const StmtNodePtr& _elseStmt):
        IStmtNode(_ifExpStmts[0].first->line), ifExpStmts(_ifExpStmts), elseStmt(_elseStmt){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_RangeFor:
    public IStmtNode {
    ExpNodePtr first, last, step;
    ExpNodePtr var;
    StmtNodePtr stmt;
    int lastLocalIdx,  stepLocalIdx; 
    StmtNode_RangeFor(const ExpNodePtr& _var, const ExpNodePtr& _first, const ExpNodePtr& _last, const ExpNodePtr& _step, const StmtNodePtr& _stmt):
        IStmtNode(_var->line), first(_first), last(_last), step(_step), var(_var), stmt(_stmt), lastLocalIdx(0), stepLocalIdx(0){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_LoopFor:
    public IStmtNode {
    StmtNodePtr initStmt;
    ExpNodePtr exp;
    StmtNodePtr bodyStmt;
    StmtNode_LoopFor(const StmtNodePtr& _initStmt, const ExpNodePtr& _exp, const StmtNodePtr& _bodyStmt):
        IStmtNode(_exp->line), initStmt(_initStmt), exp(_exp), bodyStmt(_bodyStmt){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_IteratorFor:
    public IStmtNode {
    vector<ExpNodePtr> vars, iterExps;
    StmtNodePtr stmt;
    int funcLocalIdx, stateLocalIdx;
    StmtNode_IteratorFor(const vector<ExpNodePtr>& _vars, const vector<ExpNodePtr>& _iterExps, const StmtNodePtr& _stmt):
        IStmtNode(_vars[0]->line), vars(_vars), iterExps(_iterExps), stmt(_stmt), funcLocalIdx(0), stateLocalIdx(0){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};

#endif
