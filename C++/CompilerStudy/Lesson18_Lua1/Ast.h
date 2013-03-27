#ifndef AST_H
#define AST_H

struct LuaFunctionMeta;
typedef shared_ptr<LuaFunctionMeta> LuaFunctionMetaPtr;
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
    string op;
    ExpNodePtr left, right;
    BinOpExpNode(const string& _op, const ExpNodePtr& _left, const ExpNodePtr& _right): op(_op), left(_left), right(_right){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct UnOpExpNode:
    public IExpNode {
    string op;
    ExpNodePtr exp;
    UnOpExpNode(const string& _op, const ExpNodePtr& _exp): op(_op), exp(_exp){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ConstExpNode:
    public IExpNode {
    int index;
    ConstExpNode(int _index): index(_index){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct LocalVarExpNode:
    public IExpNode {
    int index, nameIdx;
    const string& getName(const LuaFunctionMeta *meta) const;
    LocalVarExpNode(int _index, int _nameIdx): index(_index), nameIdx(_nameIdx){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct UpValueVarExpNode:
    public IExpNode {
    int index, nameIdx;
    const string& getName(const LuaFunctionMeta *meta) const;
    UpValueVarExpNode(int _index, int _nameIdx): index(_index), nameIdx(_nameIdx){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct GlobalVarExpNode:
    public IExpNode {
    string name;
    GlobalVarExpNode(const string& _name): name(_name){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct FieldAccessExpNode:
    public IExpNode {
    ExpNodePtr table;
    ExpNodePtr field;
    FieldAccessExpNode(const ExpNodePtr& _table, const ExpNodePtr& _field): table(_table), field(_field){}
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
    LuaFunctionMetaPtr meta;
    LambdaExpNode(const LuaFunctionMetaPtr& _meta): meta(_meta){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct CallExpNode:
    public IExpNode {
    ExpNodePtr func;
    vector<ExpNodePtr> params;
    CallExpNode(const ExpNodePtr& _func, vector<ExpNodePtr>&& _params): func(_func), params(_params){}
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
struct ArgsTupleExpNode:
    public IExpNode {
    virtual void acceptVisitor(IExpNodeVisitor *v) { v->visit(this);}
};
//========= IStmtNodeVisitor ===========
struct CallStmtNode;
struct AssignStmtNode;
struct BreakStmtNode;
struct ReturnStmtNode;
struct BlockStmtNode;
struct IfElseStmtNode;
struct RangeForStmtNode;
struct LoopForStmtNode;
struct IteraterForStmtNode;
struct IStmtNodeVisitor {
    virtual ~IStmtNodeVisitor(){}
    virtual void visit(CallStmtNode *v) = 0;
    virtual void visit(AssignStmtNode *v) = 0;
    virtual void visit(BreakStmtNode *v) = 0;
    virtual void visit(ReturnStmtNode *v) = 0;
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
    AssignStmtNode(const vector<ExpNodePtr>& _vars, const vector<ExpNodePtr>& _exps): vars(_vars), exps(_exps){}
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct BreakStmtNode:
    public IStmtNode {
    virtual void acceptVisitor(IStmtNodeVisitor *v) { v->visit(this);}
};
struct ReturnStmtNode:
    public IStmtNode {
    vector<ExpNodePtr> exps;
    ReturnStmtNode(const vector<ExpNodePtr>& _exps): exps(_exps){}
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
