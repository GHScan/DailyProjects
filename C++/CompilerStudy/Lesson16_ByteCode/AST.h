
#ifndef AST_H
#define AST_H

#include "Runtime.h"

struct StmtNode_IfElse;
struct StmtNode_For;
struct StmtNode_Continue;
struct StmtNode_Break;
struct StmtNode_Return;
struct StmtNode_Local;
struct StmtNode_Block;
struct StmtNode_Exp;
struct IStmtNodeVisitor
{
    virtual ~IStmtNodeVisitor() {}
    virtual void visit(StmtNode_IfElse *node) = 0;
    virtual void visit(StmtNode_For *node) = 0;
    virtual void visit(StmtNode_Continue *node) = 0;
    virtual void visit(StmtNode_Break *node) = 0;
    virtual void visit(StmtNode_Return *node) = 0;
    virtual void visit(StmtNode_Local *node) = 0;
    virtual void visit(StmtNode_Block *node) = 0;
    virtual void visit(StmtNode_Exp *node) = 0;
};

struct ExpNode_Assign;
struct ExpNode_BinaryOp;
struct ExpNode_UnaryOp;
struct ExpNode_Constant;
struct ExpNode_Variable;
struct ExpNode_Call;
struct IExpNodeVisitor
{
    virtual ~IExpNodeVisitor(){}
    virtual void visit(ExpNode_Assign *v) = 0;
    virtual void visit(ExpNode_BinaryOp *v) = 0;
    virtual void visit(ExpNode_UnaryOp *v) = 0;
    virtual void visit(ExpNode_Constant *v) = 0;
    virtual void visit(ExpNode_Variable *v) = 0;
    virtual void visit(ExpNode_Call *v) = 0;
};

struct IStmtNode
{
    virtual ~IStmtNode(){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;
struct IExpNode
{
    virtual ~IExpNode(){}
    virtual void acceptVisitor(IExpNodeVisitor *v) = 0;
};
typedef shared_ptr<IExpNode> ExpNodePtr;

struct StmtNode_IfElse:
    public IStmtNode
{
    ExpNodePtr condition;
    StmtNodePtr ifStmt;
    StmtNodePtr elseStmt;
    StmtNode_IfElse(const ExpNodePtr& c, const StmtNodePtr& _ifStmt, const StmtNodePtr& _elseStmt):
        condition(c), ifStmt(_ifStmt), elseStmt(_elseStmt){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct StmtNode_For:
    public IStmtNode
{
    ExpNodePtr s1;
    ExpNodePtr s2;
    ExpNodePtr s3;
    StmtNodePtr stmt;
    StmtNode_For(const ExpNodePtr& _s1, const ExpNodePtr& _s2, const ExpNodePtr& _s3, const StmtNodePtr& _stmt): s1(_s1), s2(_s2), s3(_s3), stmt(_stmt){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct StmtNode_Continue:
    public IStmtNode
{
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct StmtNode_Break:
    public IStmtNode
{
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct StmtNode_Return:
    public IStmtNode
{
    ExpNodePtr left;
    StmtNode_Return(const ExpNodePtr& _left): left(_left){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct StmtNode_Local:
    public IStmtNode
{
    string name;
    StmtNode_Local(const string& _name): name(_name){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct StmtNode_Block:
    public IStmtNode
{
    vector<StmtNodePtr> stmts;
    StmtNode_Block(){}
    StmtNode_Block(const vector<StmtNodePtr>& _stmts): stmts(_stmts){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct StmtNode_Exp:
    public IStmtNode
{
    ExpNodePtr exp;
    StmtNode_Exp(const ExpNodePtr& _exp): exp(_exp){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};

struct ExpNode_Assign:
    public IExpNode
{
    string name;
    ExpNodePtr right;
    ExpNode_Assign(const string& _name, const ExpNodePtr &_right):
        name(_name), right(_right){}
    ExpNode_Assign* getRightMost() {
        ExpNode_Assign *r = this;
        while (ExpNode_Assign *p = dynamic_cast<ExpNode_Assign*>(r->right.get())) {
            r = p;
        }
        return r;
    }
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ExpNode_BinaryOp:
    public IExpNode
{
    string op;
    ExpNodePtr left, right;
    ExpNode_BinaryOp(const string& _op, const ExpNodePtr &_left, const ExpNodePtr& _right):
        op(_op), left(_left), right(_right){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ExpNode_UnaryOp:
    public IExpNode
{
    string op;
    ExpNodePtr left;
    ExpNode_UnaryOp(const string& _op, const ExpNodePtr& _left):
        op(_op), left(_left){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ExpNode_Constant:
    public IExpNode
{
    Value val;
    ExpNode_Constant(const Value &_val): val(_val){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ExpNode_Variable:
    public IExpNode
{
    string name;
    ExpNode_Variable(const string& _name): name(_name){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ExpNode_Call:
    public IExpNode
{
    string name;
    vector<ExpNodePtr> params;
    ExpNode_Call(const string& _name, const vector<ExpNodePtr>& _params): name(_name), params(_params){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};

#endif
