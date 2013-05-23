
#ifndef AST_H
#define AST_H

// ExprNode
struct ExprNode_Global; 
struct ExprNode_Local;
struct ExprNode_Const;
struct ExprNode_Lambda;
struct ExprNode_ArrayConstructor;
struct ExprNode_IndexOf;
struct ExprNode_Call;
struct ExprNode_UnaryOp;
struct ExprNode_BinaryOp;
struct IExprNodeVisitor {
    virtual ~IExprNodeVisitor(){}
    virtual void visit(ExprNode_Global* node) = 0;
    virtual void visit(ExprNode_Local* node) = 0;
    virtual void visit(ExprNode_Const* node) = 0;
    virtual void visit(ExprNode_Lambda* node) = 0;
    virtual void visit(ExprNode_ArrayConstructor* node) = 0;
    virtual void visit(ExprNode_IndexOf* node) = 0;
    virtual void visit(ExprNode_Call* node) = 0;
    virtual void visit(ExprNode_UnaryOp* node) = 0;
    virtual void visit(ExprNode_BinaryOp* node) = 0;
};
struct IExprNode {
    int line;
    IExprNode(int _line): line(_line){}
    virtual void acceptVisitor(IExprNodeVisitor *v) = 0;
};
typedef shared_ptr<IExprNode> ExprNodePtr;

struct ExprNode_Global: 
    public IExprNode {
    int constIdx;
    ExprNode_Global(int line, int _constIdx): IExprNode(line), constIdx(_constIdx){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Local:
    public IExprNode {
    int localIdx;
    ExprNode_Local(int line, int _localIdx): IExprNode(line), localIdx(_localIdx){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Const:
    public IExprNode {
    int constIdx;
    ExprNode_Const(int line, int _constIdx): IExprNode(line), constIdx(_constIdx){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Lambda:
    public IExprNode {
    int metaIdx;
    ExprNode_Lambda(int line, int _metaIdx): IExprNode(line), metaIdx(_metaIdx){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_ArrayConstructor:
    public IExprNode {
    vector<ExprNodePtr> exprs;
    ExprNode_ArrayConstructor(int line, const vector<ExprNodePtr>& _exprs): 
        IExprNode(line), exprs(_exprs){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_IndexOf:
    public IExprNode {
    ExprNodePtr array, index;
    ExprNode_IndexOf(int line, const ExprNodePtr& _array, const ExprNodePtr& _index): 
        IExprNode(line), array(_array), index(_index){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_Call:
    public IExprNode {
    ExprNodePtr func;
    vector<ExprNodePtr> params;
    ExprNode_Call(int line, const ExprNodePtr& _func, const vector<ExprNodePtr>& _params): 
        IExprNode(line), func(_func), params(_params){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};
struct ExprNode_UnaryOp:
    public IExprNode {
    enum OpType {
        OT_Not, OT_Minus, OT_Len,
    };
    OpType op;
    ExprNodePtr expr;
    static OpType string2op(const string& opStr) {
        if (opStr == "not") return OT_Not;
        else if (opStr == "-") return OT_Minus;
        else if (opStr == "#") return OT_Len;
        else {
            ASSERT(0);
            return OT_Not;
        }
    }
    ExprNode_UnaryOp(int line, const string& opStr, const ExprNodePtr &_expr):
        IExprNode(line), op(string2op(opStr)), expr(_expr){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
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
    static OpType string2op(const string& opStr) {
        if (opStr == "&&") return OT_And;
        else if (opStr == "||") return OT_Or;
        else if (opStr == "<") return OT_Less;
        else if (opStr == "<=") return OT_LessEq;
        else if (opStr == ">") return OT_Greater;
        else if (opStr == ">=") return OT_GreaterEq;
        else if (opStr == "==") return OT_Equal;
        else if (opStr == "!=") return OT_NEqual;
        else if (opStr == "+") return OT_Add;
        else if (opStr == "-") return OT_Sub;
        else if (opStr == "*") return OT_Mul;
        else if (opStr == "/") return OT_Div;
        else if (opStr == "%") return OT_Mod;
        else ASSERT(0);
        return OT_And;
    }
    ExprNode_BinaryOp(int line, const string& opStr, const ExprNodePtr& _lexpr, const ExprNodePtr& _rexpr):
        IExprNode(line), op(string2op(opStr)), lexpr(_lexpr), rexpr(_rexpr){}
    virtual void acceptVisitor(IExprNodeVisitor* v){ v->visit(this);}
};

// StmtNode
struct StmtNode_Assign;
struct StmtNode_Call;
struct StmtNode_Block;
struct StmtNode_If;
struct StmtNode_For;
struct StmtNode_Break;
struct StmtNode_Continue;
struct StmtNode_Return;
struct IStmtNodeVisitor {
    virtual ~IStmtNodeVisitor() {}
    virtual void visit(StmtNode_Assign *node) = 0;
    virtual void visit(StmtNode_Call *node) = 0;
    virtual void visit(StmtNode_Block *node) = 0;
    virtual void visit(StmtNode_If *node) = 0;
    virtual void visit(StmtNode_For *node) = 0;
    virtual void visit(StmtNode_Break *node) = 0;
    virtual void visit(StmtNode_Continue *node) = 0;
    virtual void visit(StmtNode_Return *node) = 0;
};
struct IStmtNode {
    int line;
    IStmtNode(int _line): line(_line){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct StmtNode_Assign:
    public IStmtNode {
    ExprNodePtr left, right;
    StmtNode_Assign(int line, const ExprNodePtr& _left, const ExprNodePtr& _right):
        IStmtNode(line), left(_left), right(_right){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Call:
    public IStmtNode {
    ExprNodePtr callExpr;
    StmtNode_Call(int line, const ExprNodePtr& _callExpr): IStmtNode(line), callExpr(_callExpr){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Block:
    public IStmtNode {
    vector<StmtNodePtr> stmts;
    StmtNode_Block(): IStmtNode(0){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_If:
    public IStmtNode {
    ExprNodePtr expr;
    StmtNodePtr ifStmt, elseStmt;
    StmtNode_If(int line, const ExprNodePtr& _expr, const StmtNodePtr& _ifStmt, const StmtNodePtr& _elseStmt):
        IStmtNode(line), expr(_expr), ifStmt(_ifStmt), elseStmt(_elseStmt){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_For:
    public IStmtNode {
    StmtNodePtr first;
    ExprNodePtr second;
    StmtNodePtr last, body;
    StmtNode_For(int line, const StmtNodePtr& _first, const ExprNodePtr& _second, const StmtNodePtr& _last, const StmtNodePtr& _body):
        IStmtNode(line), first(_first), second(_second), last(_last), body(_body) {}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Break:
    public IStmtNode {
    StmtNode_Break(int line): IStmtNode(line){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Continue:
    public IStmtNode {
    StmtNode_Continue(int line): IStmtNode(line){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Return:
    public IStmtNode {
    ExprNodePtr expr;
    StmtNode_Return(int line, const ExprNodePtr& _expr): IStmtNode(line), expr(_expr){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};

#endif
