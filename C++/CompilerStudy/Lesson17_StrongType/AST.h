
#ifndef AST_H
#define AST_H

struct IType;
//////////
struct ExpNode_ConstantInt ;
struct ExpNode_ConstantString ;
struct ExpNode_Variable ;
struct ExpNode_Conversion ;
struct ExpNode_BinaryOp ;
struct ExpNode_UnaryOp ;
struct ExpNode_Addr ;
struct ExpNode_Unref ;
struct ExpNode_Field ;
struct ExpNode_ArrayElem ;
struct ExpNode_Call ;
struct ExpNode_Assign ;
struct IExpNodeVisitor
{
    virtual ~IExpNodeVisitor() {}
    virtual void visit(ExpNode_ConstantInt* node) = 0;
    virtual void visit(ExpNode_ConstantString* node) = 0;
    virtual void visit(ExpNode_Variable* node) = 0;
    virtual void visit(ExpNode_Conversion* node) = 0;
    virtual void visit(ExpNode_BinaryOp* node) = 0;
    virtual void visit(ExpNode_UnaryOp* node) = 0;
    virtual void visit(ExpNode_Addr* node) = 0;
    virtual void visit(ExpNode_Unref* node) = 0;
    virtual void visit(ExpNode_Field* node) = 0;
    virtual void visit(ExpNode_ArrayElem* node) = 0;
    virtual void visit(ExpNode_Call* node) = 0;
    virtual void visit(ExpNode_Assign* node) = 0;
};
struct IExpNode
{
    IType *type;
    virtual ~IExpNode(){}
    virtual void acceptVisitor(IExpNodeVisitor *v) = 0;
};
typedef shared_ptr<IExpNode> ExpNodePtr;

struct ExpNode_ConstantInt:
    public IExpNode
{
    int value;
    ExpNode_ConstantInt(int v): value(v){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_ConstantString:
    public IExpNode
{
    const char *str;
    ExpNode_ConstantString(const string& s);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Variable:
    public IExpNode
{
    bool isGlobal;
    int offset;
    ExpNode_Variable(const string& name);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Conversion:
    public IExpNode
{
    ExpNodePtr left;
    ExpNode_Conversion(const ExpNodePtr& _left, IType* type);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_BinaryOp:
    public IExpNode
{
    enum BinOp
    {
        BO_Add,
        BO_Sub,
        BO_Mul,
        BO_Div,
        BO_Mod,

        BO_Less,
        BO_LessEq,
        BO_Equal,
        BO_Greater,
        BO_GreaterEq,

        BO_And,
        BO_Or,
    };
    static BinOp string2op(const string& str);
    BinOp op;
    ExpNodePtr left, right;
    ExpNode_BinaryOp(const ExpNodePtr& _left, const ExpNodePtr& _right, BinOp _op);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_UnaryOp:
    public IExpNode
{
    enum UnaryOp
    {
        UO_Not,
        UO_Inc,
        UO_Dec,
    };
    static UnaryOp string2op(const string& str);
    UnaryOp op;
    ExpNodePtr left;
    ExpNode_UnaryOp(const ExpNodePtr& _left, UnaryOp _op);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Addr:
    public IExpNode
{
    ExpNodePtr left;
    ExpNode_Addr(const ExpNodePtr& _left);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Unref:
    public IExpNode
{
    ExpNodePtr left;
    ExpNode_Unref(const ExpNodePtr& _left);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Field:
    public IExpNode
{
    ExpNodePtr left;
    const char *fieldName;
    ExpNode_Field(const ExpNodePtr& _left, const string& _fieldName);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_ArrayElem:
    public IExpNode
{
    ExpNodePtr left;
    ExpNodePtr right;
    ExpNode_ArrayElem(const ExpNodePtr& _left, const ExpNodePtr& _right);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Call:
    public IExpNode
{
    const char *name;
    vector<ExpNodePtr> args;
    explicit ExpNode_Call(const string& _name, const vector<ExpNodePtr>& _args);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Assign:
    public IExpNode
{
    ExpNodePtr left;
    ExpNodePtr right;
    ExpNode_Assign(const ExpNodePtr& _left, const ExpNodePtr& _right);
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};

//////////
struct StmtNode_Exp ;
struct StmtNode_Block ;
struct StmtNode_DefineLocal ;
struct StmtNode_Break ;
struct StmtNode_Continue ;
struct StmtNode_Return ;
struct StmtNode_For ;
struct StmtNode_IfElse ;
struct StmtNode_Switch ;
struct IStmtNodeVisitor
{
    virtual ~IStmtNodeVisitor() {}
    virtual void visit(StmtNode_Exp* node) = 0;
    virtual void visit(StmtNode_Block* node) = 0;
    virtual void visit(StmtNode_DefineLocal* node) = 0;
    virtual void visit(StmtNode_Break* node) = 0;
    virtual void visit(StmtNode_Continue* node) = 0;
    virtual void visit(StmtNode_Return* node) = 0;
    virtual void visit(StmtNode_For* node) = 0;
    virtual void visit(StmtNode_IfElse* node) = 0;
    virtual void visit(StmtNode_Switch* node) = 0;
};
struct IStmtNode
{
    IType *type;
    virtual ~IStmtNode(){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct StmtNode_Exp:
    public IStmtNode
{
    ExpNodePtr exp;
    StmtNode_Exp(const ExpNodePtr& _exp);
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Block:
    public IStmtNode
{
    vector<StmtNodePtr> stmts;
    StmtNode_Block(const vector<StmtNodePtr>& _stmts);
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_DefineLocal:
    public IStmtNode
{
    const char *name;
    IType *type;
    StmtNode_DefineLocal(const string& _name, IType *_type);
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Break:
    public IStmtNode
{
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Continue:
    public IStmtNode
{
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Return:
    public IStmtNode
{
    ExpNodePtr exp;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_For:
    public IStmtNode
{
    StmtNodePtr s1;
    ExpNodePtr s2;
    ExpNodePtr s3;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_IfElse:
    public IStmtNode
{
    ExpNodePtr exp;
    StmtNodePtr ifStmt;
    StmtNodePtr elseStmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Switch:
    public IStmtNode
{
    ExpNodePtr exp;
    // TODO: to support the literal case !
    map<int, StmtNodePtr> caseMap; 
    StmtNodePtr defaultStmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};

#endif
