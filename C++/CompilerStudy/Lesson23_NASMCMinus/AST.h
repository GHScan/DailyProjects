
#ifndef AST_H
#define AST_H

struct ExprNode_StringLiteral;
struct ExprNode_IntLiteral;
struct ExprNode_FloatLiteral;
struct ExprNode_Variable;
struct ExprNode_Assignment;
struct ExprNode_BinaryOp;
struct ExprNode_UnaryOp;
struct ExprNode_TypeCast;
struct ExprNode_Call;
struct IExprNodeVisitor {
    virtual ~IExprNodeVisitor(){}
    virtual void visit(ExprNode_StringLiteral *node) = 0;
    virtual void visit(ExprNode_IntLiteral *node) = 0;
    virtual void visit(ExprNode_FloatLiteral *node) = 0;
    virtual void visit(ExprNode_Variable *node) = 0;
    virtual void visit(ExprNode_Assignment *node) = 0;
    virtual void visit(ExprNode_BinaryOp *node) = 0;
    virtual void visit(ExprNode_UnaryOp *node) = 0;
    virtual void visit(ExprNode_TypeCast *node) = 0;
    virtual void visit(ExprNode_Call *node) = 0;
};

struct ExprNode {
    virtual ~ExprNode(){}
    virtual void acceptVisitor(IExprNodeVisitor *v) = 0;
};
typedef shared_ptr<ExprNode> ExprNodePtr;

struct ExprNode_StringLiteral: public ExprNode {
    string str;
    ExprNode_StringLiteral(const string& _str): str(_str){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_IntLiteral: public ExprNode {
    int number;
    ExprNode_IntLiteral(int _number): number(_number){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_FloatLiteral: public ExprNode {
    float number;
    ExprNode_FloatLiteral(float _number): number(_number){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_Variable: public ExprNode {
    string name;
    ExprNode_Variable(const string& _name): name(_name){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_Assignment: public ExprNode {
    string left;
    ExprNodePtr right;
    ExprNode_Assignment(const string& _left, const ExprNodePtr& _right): left(_left), right(_right){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_BinaryOp: public ExprNode {
    string op;
    ExprNodePtr left, right;
    ExprNode_BinaryOp(const string& _op, const ExprNodePtr &_left, const ExprNodePtr &_right): op(_op), left(_left), right(_right){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_UnaryOp: public ExprNode {
    string op;
    ExprNodePtr expr;
    ExprNode_UnaryOp(const string &_op, const ExprNodePtr &_expr): op(_op), expr(_expr){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_TypeCast: public ExprNode {
    string destType;
    ExprNodePtr expr;
    ExprNode_TypeCast(const string &_destType, const ExprNodePtr &_expr): destType(_destType), expr(_expr){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};
struct ExprNode_Call: public ExprNode {
    string funcName;
    vector<ExprNodePtr> args;
    ExprNode_Call(const string &_funcName, const vector<ExprNodePtr> &_args): funcName(_funcName), args(_args){}
    virtual void acceptVisitor(IExprNodeVisitor *v) { v->visit(this); }
};

struct StmtNode_Block;
struct StmtNode_Stmts;
struct StmtNode_Expr;
struct StmtNode_DefineVariable;
struct StmtNode_Continue;
struct StmtNode_Break;
struct StmtNode_Return;
struct StmtNode_IfThenElse;
struct StmtNode_For;
struct IStmtNodeVisitor {
    virtual ~IStmtNodeVisitor(){}
    virtual void visit(StmtNode_Block *node) = 0;
    virtual void visit(StmtNode_Stmts *node) = 0;
    virtual void visit(StmtNode_Expr *node) = 0;
    virtual void visit(StmtNode_DefineVariable *node) = 0;
    virtual void visit(StmtNode_Continue *node) = 0;
    virtual void visit(StmtNode_Break *node) = 0;
    virtual void visit(StmtNode_Return *node) = 0;
    virtual void visit(StmtNode_IfThenElse *node) = 0;
    virtual void visit(StmtNode_For *node) = 0;
};

struct StmtNode {
    virtual ~StmtNode(){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<StmtNode> StmtNodePtr;

struct StmtNode_Block: public StmtNode {
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Stmts: public StmtNode {
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Expr: public StmtNode {
    ExprNodePtr expr;
    StmtNode_Expr(const ExprNodePtr &_expr): expr(_expr){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_DefineVariable: public StmtNode {
    string type, name;
    StmtNode_DefineVariable(const string &_type, const string &_name): type(_type), name(_name){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Continue: public StmtNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Break: public StmtNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_Return: public StmtNode {
    ExprNodePtr expr;
    StmtNode_Return(const ExprNodePtr &_expr): expr(_expr){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_IfThenElse: public StmtNode {
    ExprNodePtr cond;
    StmtNodePtr thenStmt, elseStmt;
    StmtNode_IfThenElse(const ExprNodePtr &_cond, const StmtNodePtr &_thenStmt, const StmtNodePtr &_elseStmt): 
        cond(_cond), thenStmt(_thenStmt), elseStmt(_elseStmt){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};
struct StmtNode_For: public StmtNode {
    StmtNodePtr first;
    ExprNodePtr second, third;
    StmtNodePtr body;
    StmtNode_For(const StmtNodePtr &_first, const ExprNodePtr &_second, const ExprNodePtr &_third, const StmtNodePtr &_body):
        first(_first), second(_second), third(_third), body(_body){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) {v->visit(this);}
};

#endif
