
#ifndef AST_H
#define AST_H

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
    virtual void acceptVisitor(IExpNodeVisitor *v) = 0;
};
typedef shared_ptr<IExpNode> ExpNodePtr;

struct ExpNode_UnaryOp:
    public IExpNode {
    enum OpType {
        OT_Not,
        OT_Minus,
        OT_Len,
    };
    ExpNodePtr exp;
    OpType op;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_BinaryOp:
    public IExpNode {
    enum OpType {
        OT_And, OT_Or,
        OT_Less, OT_LessEq,
        OT_Greater, OT_GreaterEq,
        OT_Equal, OT_NEqual,
        OT_Add, OT_Sub, OT_Mul, OT_Div, OT_Mod,
        OT_Pow,
        OT_Concat,
    };
    ExpNodePtr lexp, rexp;
    OpType op;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Const:
    public IExpNode {
    int constIdx;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_LocalVar:
    public IExpNode {
    int localIdx;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_UpValueVar:
    public IExpNode {
    int upValueIdx;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_GlobalVar:
    public IExpNode {
    string name;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_FieldAccess:
    public IExpNode {
    ExpNodePtr talbe, field;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_TableConstructor:
    public IExpNode {
    vector<ExpNodePtr> array;
    vector<pair<ExpNodePtr, ExpNodePtr> > dict;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Lambda:
    public IExpNode {
    // TODO
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Call:
    public IExpNode {
    ExpNodePtr func;
    vector<ExpNodePtr> params;
    virtual void acceptVisitor(IExpNodeVisitor *v) {v->visit(this);}
};
struct ExpNode_Args:
    public IExpNode {
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
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct StmtNode_Call:
    public IStmtNode {
    ExpNodePtr callExp;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Assign:
    public IStmtNode {
    vector<ExpNodePtr> lvalues, rvalues;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Break:
    public IStmtNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Continue:
    public IStmtNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Return:
    public IStmtNode {
    vector<ExpNodePtr> exps;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Block:
    public IStmtNode {
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_IfElse:
    public IStmtNode {
    vector<pair<ExpNodePtr, StmtNodePtr> > ifExpStmts;
    StmtNodePtr elseStmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_RangeFor:
    public IStmtNode {
    ExpNodePtr first, last, step;
    ExpNodePtr var;
    StmtNodePtr stmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_LoopFor:
    public IStmtNode {
    StmtNodePtr initStmt;
    ExpNodePtr exp;
    StmtNodePtr bodyStmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_IteratorFor:
    public IStmtNode {
    vector<ExpNodePtr> vars, iterExps;
    StmtNodePtr stmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};

#endif
