#ifndef AST_H
#define AST_H

struct LuaFunctionMeta;
//========= IExpNodeVisitor ===========
struct BinOpExpNode;
struct UnOpExpNode;
struct ConstExpNode;
struct LocalVarExpNode;
struct UpValueVarExpNode;
struct GlobalVarExpNode;
struct FieldAccessExpNode;
struct TableConstructorExpNode;
struct LambdaExpNode;
struct CallExpNode;
struct ArgsTupleExpNode;
struct IExpNodeVisitor {
    virtual ~IExpNodeVisitor(){}
    virtual void visit(BinOpExpNode *v) = 0; 
    virtual void visit(UnOpExpNode *v) = 0;
    virtual void visit(ConstExpNode *v) = 0;
    virtual void visit(LocalVarExpNode *v) = 0;
    virtual void visit(UpValueVarExpNode *v) = 0;
    virtual void visit(GlobalVarExpNode *v) = 0;
    virtual void visit(FieldAccessExpNode *v) = 0;
    virtual void visit(TableConstructorExpNode *v) = 0;
    virtual void visit(LambdaExpNode *v) = 0;
    virtual void visit(CallExpNode *v) = 0;
    virtual void visit(ArgsTupleExpNode *v) = 0;
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
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct UnOpExpNode:
    public IExpNode {
    ExpNodePtr cexp;
    string op;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ConstExpNode:
    public IExpNode {
    int index;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct LocalVarExpNode:
    public IExpNode {
    int index, nameIdx;
    const string& getName(const LuaFunctionMeta *meta) const;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct UpValueVarExpNode:
    public IExpNode {
    int index, nameIdx;
    const string& getName(const LuaFunctionMeta *meta) const;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct GlobalVarExpNode:
    public IExpNode {
    string name;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct FieldAccessExpNode:
    public IExpNode {
    ExpNodePtr value;
    ExpNodePtr field;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct TableConstructorExpNode:
    public IExpNode {
    vector<ExpNodePtr> vec;
    vector<pair<ExpNodePtr, ExpNodePtr> > hashTable;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct LambdaExpNode:
    public IExpNode {
    LuaFunctionMeta *meta;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct CallExpNode:
    public IExpNode {
    ExpNodePtr func;
    vector<ExpNodePtr> params;
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ArgsTupleExpNode:
    public IExpNode {
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
//========= IStmtNodeVisitor ===========
struct CallStmtNode;
struct AssignStmtNode;
struct BlockStmtNode;
struct IfElseStmtNode;
struct RangeForStmtNode;
struct LoopForStmtNode;
struct IteraterForStmtNode;
struct IStmtNodeVisitor {
    virtual ~IStmtNodeVisitor(){}
    virtual void visit(CallStmtNode *v) = 0;
    virtual void visit(AssignStmtNode *v) = 0;
    virtual void visit(BlockStmtNode *v) = 0;
    virtual void visit(IfElseStmtNode *v) = 0;
    virtual void visit(RangeForStmtNode *v) = 0;
    virtual void visit(LoopForStmtNode *v) = 0;
    virtual void visit(IteraterForStmtNode *v) = 0;
};
struct IStmtNode {
    virtual ~IStmtNode(){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) = 0;
};
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct CallStmtNode:
    public IStmtNode {
    ExpNodePtr exp;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct AssignStmtNode:
    public IStmtNode {
    vector<ExpNodePtr> vars;
    vector<ExpNodePtr> exps;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct BlockStmtNode:
    public IStmtNode {
    vector<StmtNodePtr> stmts;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct IfElseStmtNode:
    public IStmtNode {
    vector<pair<ExpNodePtr, StmtNodePtr> > ifExpStmtList;
    StmtNodePtr elseStmt;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct RangeForStmtNode:
    public IStmtNode {
    int first, last, step;
    int index, nameIdx;
    StmtNodePtr stmt;
    const string& getName(const LuaFunctionMeta *meta) const;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct LoopForStmtNode:
    public IStmtNode {
    StmtNodePtr stmtPre;
    ExpNodePtr exp;
    StmtNodePtr stmtBody;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct IteraterForStmtNode:
    public IStmtNode {
    vector<int> indexs;
    vector<int> nameIdxs;
    ExpNodePtr iterExp;
    StmtNodePtr stmt;
    const string& getName(const LuaFunctionMeta *meta, int idx) const;
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};

#endif
