#ifndef AST_H
#define AST_H

//========= IExpNodeVisitor ===========
struct IExpNodeVisitor {
    virtual ~IExpNodeVisitor(){}
    virtual void accept() = 0;
};
struct IExpNode {
    virtual ~IExpNode(){}
    virtual void acceptVisitor(IExpNodeVisitor *v) = 0;
};
typedef shared_ptr<IExpNode>  ExpNodePtr;

struct BinOpExpNode: 
    public IExpNode {
    ExpNodePtr left, right;
    string op;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct UnOpExpNode:
    public IExpNode {
    ExpNodePtr cexp;
    string op;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct ConstExpNode:
    public IExpNode {
    int index;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct LocalVarExpNode:
    public IExpNode {
    int index;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct UpValueVarExpNode:
    public IExpNode {
    int index;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct GlobalVarExpNode:
    public IExpNode {
    string name;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct FieldAccessExpNode:
    public IExpNode {
    ExpNodePtr value;
    ExpNodePtr field;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct TableConstructorExpNode:
    public IExpNode {
    vector<ExpNodePtr> vec;
    vector<pair<ExpNodePtr, ExpNodePtr> > hashTable;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct LambdaExpNode:
    public IExpNode {
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct CallExpNode:
    public IExpNode {
    ExpNodePtr func;
    vector<ExpNodePtr> params;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
struct ArgsTupleExpNode:
    public IExpNode {
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->accept(this);}
};
//========= IStmtNodeVisitor ===========
struct IStmtNodeVisitor {
    virtual ~IStmtNodeVisitor(){}
    virtual void accept() = 0;
};
struct IStmtNode {
    virtual ~IStmtNode(){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct ExpStmtNode:
    public IStmtNode {
    ExpNodePtr exp;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->accept(this);}
};
struct AssignStmtNode:
    public IStmtNode {
    vector<ExpNodePtr> vars;
    vector<ExpNodePtr> exps;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->accept(this);}
};
struct BlockStmtNode:
    public IStmtNode {
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->accept(this);}
};
struct IfElseStmtNode:
    public IStmtNode {
    vector<ExpNodePtr, StmtNodePtr> ifExpStmtList;
    StmtNodePtr elseStmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->accept(this);}
};
struct RangeForStmtNode:
    public IStmtNode {
    int first, last, step;
    string varName;
    StmtNodePtr stmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->accept(this);}
};
struct LoopForStmtNode:
    public IStmtNode {
    StmtNodePtr stmt1;
    ExpNodePtr exp;
    StmtNodePtr stmt2;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->accept(this);}
};
struct IteraterForStmtNode:
    public IStmtNode {
    vector<string> varNameList;
    ExpNodePtr iterExp;
    StmtNodePtr stmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->accept(this);}
};

#endif
