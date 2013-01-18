#ifndef PARSER_H
#define PARSER_H

#include "common.h"

struct ExprNode_Const;
struct ExprNode_ID;
struct ExprNode_Call;
struct ExprNode_UnaryOp;
struct ExprNode_BinaryOp;
struct ExprNode_Assign;
struct StmtNode_ExprOnly;
struct StmtNode_IfElse;
struct StmtNode_While;
struct StmtNode_Stmts;
struct StmtNode_Local;
struct StmtNode_Return;
struct StmtNode_Break;
struct StmtNode_Continue;
struct ISyntaxTreeVisitor
{
    virtual ~ISyntaxTreeVisitor() {}
    virtual void visit(ExprNode_Const *node) = 0;
    virtual void visit(ExprNode_ID *node) = 0;
    virtual void visit(ExprNode_Call *node) = 0;
    virtual void visit(ExprNode_UnaryOp *node) = 0;
    virtual void visit(ExprNode_BinaryOp *node) = 0;
    virtual void visit(ExprNode_Assign *node) = 0;
    virtual void visit(StmtNode_ExprOnly *node) = 0;
    virtual void visit(StmtNode_IfElse *node) = 0;
    virtual void visit(StmtNode_While *node) = 0;
    virtual void visit(StmtNode_Stmts *node) = 0;
    virtual void visit(StmtNode_Local *node) = 0;
    virtual void visit(StmtNode_Return *node) = 0;
    virtual void visit(StmtNode_Break *node) = 0;
    virtual void visit(StmtNode_Continue *node) = 0;
};
struct ExprNode
{
    virtual ~ExprNode(){}
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) = 0;
};
typedef std::shared_ptr<ExprNode> ExprNodePtr;
struct StmtNode
{
    virtual ~StmtNode(){}
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) = 0;
};
typedef std::shared_ptr<StmtNode> StmtNodePtr;

struct ExprNode_Const:
    public ExprNode
{
    ValuePtr value;
    ExprNode_Const(const ValuePtr& value){ this->value = value ;}
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this); }
};
struct ExprNode_ID:
    public ExprNode
{
    std::string name;
    ExprNode_ID(const std::string& name) { this->name = name; }
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this); }
};
struct ExprNode_Call:
    public ExprNode
{
    std::string fname;
    std::vector<ExprNodePtr> params;
    ExprNode_Call(const std::string& fname) { this->fname = fname; }
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this); }
};
struct ExprNode_UnaryOp:
    public ExprNode
{
    std::string op;
    ExprNodePtr child;
    ExprNode_UnaryOp(const std::string& op){this->op = op; }
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this); }
};
struct ExprNode_BinaryOp:
    public ExprNode
{
    std::string op;
    ExprNodePtr left, right;
    ExprNode_BinaryOp(const std::string& op){ this->op = op; }
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this); }
};
struct ExprNode_Assign:
    public ExprNode
{
    std::string left;
    ExprNodePtr right;
    ExprNode_Assign(const std::string& left){this->left = left;}
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this); }
};
struct StmtNode_ExprOnly:
    public StmtNode
{
    ExprNodePtr expr;
    StmtNode_ExprOnly(){}
    StmtNode_ExprOnly(ExprNodePtr expr) { this->expr = expr; }
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this); }
};
struct StmtNode_IfElse:
    public StmtNode
{
    ExprNodePtr expr;
    StmtNodePtr ifStmt, elseStmt;
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this);}
};
struct StmtNode_While:
    public StmtNode
{
    ExprNodePtr expr;
    StmtNodePtr stmt;
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this);}
};
struct StmtNode_Stmts:
    public StmtNode
{
    std::vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this);}
};
struct StmtNode_Local:
    public StmtNode
{
    std::string left;
    ExprNodePtr right;
    StmtNode_Local(){}
    StmtNode_Local(const std::string& left){ this->left = left ;}
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this);}
};
struct StmtNode_Return:
    public StmtNode
{
    ExprNodePtr right;
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this);}
};
struct StmtNode_Break:
    public StmtNode
{
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this);}
};
struct StmtNode_Continue:
    public StmtNode
{
    virtual void acceptVisitor(ISyntaxTreeVisitor *v) { v->visit(this);}
};
struct FuncSyntax
{
    std::string name;
    StmtNodePtr args;
    StmtNodePtr stmts;
    FuncSyntax(const std::string& name){ this->name = name; }
};
typedef std::shared_ptr<FuncSyntax> FuncSyntaxPtr;

class Parser
{
public:
    Parser();
    ~Parser();
    bool parse(const std::string& fname, const std::string& src);
    const std::vector<FuncSyntaxPtr> &getFuncs() const;
private:
    class ParserImpl *m_impl;
};

#endif
