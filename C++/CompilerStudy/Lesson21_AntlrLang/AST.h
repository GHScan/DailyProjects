
#ifndef AST_H
#define AST_H

struct IASTNode {
    virtual ~IASTNode(){}
};
typedef shared_ptr<IASTNode> ASTNodePtr;

// ExprNode
struct IExprNodeVisitor {
    virtual ~IExprNodeVisitor(){}
    virtual void visit() = 0;
};
struct IExprNode:
    public IASTNode {
    virtual void acceptVisitor(IExprNodeVisitor *v) = 0;
};
typedef shared_ptr<IExprNode> ExprNodePtr;

struct ExprNode_Global: 
    public IExprNode {
    string name;
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Local:
    public IExprNode {
    int stackIdx;
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Const:
    public IExprNode {
    int constIdx;
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Lambda:
    public IExprNode {
    int metaIdx; //TODO
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_ArrayConstructor:
    public IExprNode {
    vector<ExprNodePtr> exprs;
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_IndexOf:
    public IExprNode {
    ExprNodePtr array, index;
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Call:
    public IExprNode {
    ExprNodePtr func;
    vector<ExprNodePtr> params;
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_UnaryOp:
    public IExprNode {
    enum OpType {
        OT_Inc, OT_Dec, OT_Not, OT_Minus,
    };
    OpType op;
    ExprNodePtr expr;
};
struct ExprNode_BinaryOp:
    public IExprNode {
    enum OpType {
        OT_And, OT_Or,
        OT_Less, OT_LessEq, OT_Greater, OT_GreaterEq, OT_Equal, OT_NEqual,
        OT_Add, OT_Sub,
        OT_Mul, OT_Div, OT_Mod, OT_Pow, 
    };
    OpType op;
    ExprNodePtr lexpr, rexpr;
};

// StmtNode
struct IStmtNodeVisitor {
    virtual ~IStmtNodeVisitor() {}
    virtual void visit() = 0;
};
struct IStmtNode:
    public IASTNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct StmtNode_Assign:
    public IStmtNode {
    ExprNodePtr left, right;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Call:
    public IStmtNode {
    ExprNodePtr callExp;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Block:
    public IStmtNode {
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_If:
    public IStmtNode {
    ExprNodePtr expr;
    StmtNodePtr ifStmt, elseStmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_For:
    public IStmtNode {
    StmtNodePtr firstStmt;
    ExprNodePtr second, third;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Break:
    public IStmtNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Continue:
    public IStmtNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Return:
    public IStmtNode {
    ExprNodePtr expr;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};

#endif
