
#ifndef AST_H
#define AST_H

struct IType;
//////////
struct ExpNode_ConstantInt ;
struct ExpNode_ConstantLiteral ;
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
struct ExpNode_Sizeof ;
struct IExpNodeVisitor
{
    virtual ~IExpNodeVisitor() {}
    virtual void visit(ExpNode_ConstantInt* node) = 0;
    virtual void visit(ExpNode_ConstantLiteral* node) = 0;
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
    virtual void visit(ExpNode_Sizeof* node) = 0;
};
struct IExpNode
{
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
struct ExpNode_ConstantLiteral:
    public IExpNode
{
    string str;
    ExpNode_ConstantLiteral(const string& s): str(s){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Variable:
    public IExpNode
{
    string name;
    ExpNode_Variable(const string& _name): name(_name){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Conversion:
    public IExpNode
{
    ExpNodePtr left;
    IType *type;
    ExpNode_Conversion(const ExpNodePtr& _left, IType* _type): left(_left), type(_type){}
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
    static BinOp string2op(const string& str)
    {
        if (str == "+") return BO_Add;
        else if (str == "-") return BO_Sub;
        else if (str == "*") return BO_Mul;
        else if (str == "/") return BO_Div;
        else if (str == "%") return BO_Mod;

        else if (str == "<") return BO_Less;
        else if (str == "<=") return BO_LessEq;
        else if (str == "==") return BO_Equal;
        else if (str == ">") return BO_Greater;
        else if (str == ">=") return BO_GreaterEq;

        else if (str == "&&") return BO_And;
        else if (str == "||") return BO_Or;
        else ASSERT(0);
        return BO_Add;
    }

    BinOp op;
    ExpNodePtr left, right;
    ExpNode_BinaryOp(const ExpNodePtr& _left, const ExpNodePtr& _right, BinOp _op): left(_left), right(_right), op(_op){}
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
    static UnaryOp string2op(const string& str)
    {
        if (str == "!") return UO_Not;
        else if (str == "++") return UO_Inc;
        else if (str == "--") return UO_Dec;
        else ASSERT(0);
        return UO_Not;
    }

    UnaryOp op;
    ExpNodePtr left;
    ExpNode_UnaryOp(const ExpNodePtr& _left, UnaryOp _op): left(_left), op(_op){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Addr:
    public IExpNode
{
    ExpNodePtr left;
    ExpNode_Addr(const ExpNodePtr& _left): left(_left){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Unref:
    public IExpNode
{
    ExpNodePtr left;
    ExpNode_Unref(const ExpNodePtr& _left): left(_left){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Field:
    public IExpNode
{
    ExpNodePtr left;
    string fieldName;
    bool dot;
    ExpNode_Field(const ExpNodePtr& _left, const string& _fieldName, bool _dot): 
        left(_left), fieldName(_fieldName), dot(_dot){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_ArrayElem:
    public IExpNode
{
    ExpNodePtr left;
    ExpNodePtr right;
    ExpNode_ArrayElem(const ExpNodePtr& _left, const ExpNodePtr& _right): left(_left), right(_right){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Call:
    public IExpNode
{
    string name;
    vector<ExpNodePtr> args;
    ExpNode_Call(const string& _name, const vector<ExpNodePtr>& _args): name(_name), args(_args){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Assign:
    public IExpNode
{
    ExpNodePtr left;
    ExpNodePtr right;
    ExpNode_Assign(const ExpNodePtr& _left, const ExpNodePtr& _right): left(_left), right(_right){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this); }
};
struct ExpNode_Sizeof:
    public IExpNode
{
    ExpNodePtr left;
    ExpNode_Sizeof(const ExpNodePtr& _left): left(_left){}
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
    virtual ~IStmtNode(){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct StmtNode_Exp:
    public IStmtNode
{
    ExpNodePtr exp;
    StmtNode_Exp(const ExpNodePtr& _exp): exp(_exp){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Block:
    public IStmtNode
{
    vector<StmtNodePtr> stmts;
    StmtNode_Block(){}
    StmtNode_Block(const vector<StmtNodePtr>& _stmts): stmts(_stmts){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_DefineLocal:
    public IStmtNode
{
    string name;
    IType *type;
    StmtNode_DefineLocal(const string& _name, IType *_type): name(_name), type(_type){}
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
    StmtNode_Return(const ExpNodePtr& _exp): exp(_exp){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_For:
    public IStmtNode
{
    ExpNodePtr s1;
    ExpNodePtr s2;
    ExpNodePtr s3;
    StmtNodePtr body;
    StmtNode_For(const ExpNodePtr& _s1, const ExpNodePtr& _s2, const ExpNodePtr& _s3, const StmtNodePtr& _body): 
        s1(_s1), s2(_s2), s3(_s3), body(_body){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_IfElse:
    public IStmtNode
{
    ExpNodePtr exp;
    StmtNodePtr ifStmt;
    StmtNodePtr elseStmt;
    StmtNode_IfElse(const ExpNodePtr& _exp, const StmtNodePtr& _ifStmt, const StmtNodePtr& _elseStmt): exp(_exp), ifStmt(_ifStmt), elseStmt(_elseStmt){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};
struct StmtNode_Switch:
    public IStmtNode
{
    ExpNodePtr exp;
    // TODO: to support the literal case !
    map<int, StmtNodePtr> caseMap; 
    StmtNodePtr defaultStmt;
    StmtNode_Switch(const ExpNodePtr& _exp): exp(_exp){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this); }
};

#endif
