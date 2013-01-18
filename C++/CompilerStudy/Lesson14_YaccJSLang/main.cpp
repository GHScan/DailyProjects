
#include "pch.h"

#include <time.h>

#include "AST.h"
#include "Runtime.h"
#include "LuaCodeGen.h"

class ExpNodeVisitor_Eval:
    public IExpNodeVisitor
{
public:
    ExpNodeVisitor_Eval(StackFrame* frame, ExpNodePtr exp):
        m_frame(frame)
    {
        exp->acceptVisitor(this);
    }
    const Value& getValue() const { return m_value; }
private:
    virtual void visit(ExpNode_Assign *v) 
    {
        v->right->acceptVisitor(this);
        m_frame->setValue(v->name, m_value);
    }
    virtual void visit(ExpNode_BinaryOp *v) 
    {
        v->left->acceptVisitor(this);
        Value lv = m_value;
        if (v->op == "&&") {
            if (!lv.toBoolean()) m_value = Value::createBoolean(false);
            else {
                v->right->acceptVisitor(this);
                Value rv = m_value;
                m_value = Value::createBoolean(rv.toBoolean());
            }
            return;
        }
        else if (v->op == "||") {
            if (lv.toBoolean()) m_value = Value::createBoolean(true);
            else {
                v->right->acceptVisitor(this);
                Value rv = m_value;
                m_value = Value::createBoolean(rv.toBoolean());
            }
            return;
        }
        v->right->acceptVisitor(this);
        Value rv = m_value;
        if (v->op == "+") m_value = lv + rv;
        else if (v->op == "-") m_value = lv - rv;
        else if (v->op == "*") m_value = lv * rv;
        else if (v->op == "/") m_value = lv / rv;
        else if (v->op == "%") m_value = lv % rv;
        else if (v->op == "<") m_value = Value::createBoolean(lv < rv);
        else if (v->op == ">") m_value = Value::createBoolean(rv < lv);
        else if (v->op == "==") m_value = Value::createBoolean(lv == rv);
        else if (v->op == "!=") m_value = Value::createBoolean(!(lv == rv));
        else if (v->op == "<=") m_value = Value::createBoolean(!(rv < lv));
        else if (v->op == ">=") m_value = Value::createBoolean(!(lv < rv));
        else ASSERT(0);
    }
    virtual void visit(ExpNode_UnaryOp *v) 
    {
    }
    virtual void visit(ExpNode_Constant *v) 
    {
        m_value = v->val;
    }
    virtual void visit(ExpNode_Variable *v) 
    {
        m_value = m_frame->getValue(v->name);
    }
    virtual void visit(ExpNode_Call *v) 
    {
        auto f = GlobalEnvironment::instance()->getFunc(v->name);
        vector<Value> params;
        for (auto &exp : v->params) {
            exp->acceptVisitor(this);
            params.push_back(m_value);
        }
        m_value = f->call(params);
    }
private:
    Value m_value;
    StackFrame *m_frame;
};
class StmtNodeVisitor_Execute:
    public IStmtNodeVisitor
{
public:
    StmtNodeVisitor_Execute(const ASTFunction* func, const vector<Value>& args):
        m_args(args), m_continue(false), m_break(false), m_return(false)
    {
        ASSERT(args.size() == func->argc);
        func->stmt->acceptVisitor(this);
    }
    const Value& getRet() const { return m_ret; }
private:
    virtual void visit(StmtNode_IfElse *node) 
    {
        Value v = ExpNodeVisitor_Eval(&m_frame, node->condition).getValue();
        if (v.toBoolean()) {
            if (node->ifStmt) node->ifStmt->acceptVisitor(this);
        }
        else {
            if (node->elseStmt) node->elseStmt->acceptVisitor(this);
        }
    }
    virtual void visit(StmtNode_For *node) 
    {
        if (node->s1) ExpNodeVisitor_Eval(&m_frame, node->s1);
        while (!m_break && !m_return) {
            bool go = true;
            if (node->s2) {
                go = ExpNodeVisitor_Eval(&m_frame, node->s2).getValue().toBoolean();
            }
            if (!go) break;
            if (node->stmt) node->stmt->acceptVisitor(this);
            if (node->s3) ExpNodeVisitor_Eval(&m_frame, node->s3);
            m_continue = false;
        }
        m_break = false;
    }
    virtual void visit(StmtNode_Continue *node) 
    {
        m_continue = true;
    }
    virtual void visit(StmtNode_Break *node) 
    {
        m_break = true;
    }
    virtual void visit(StmtNode_Return *node) 
    {
        m_return = true;
        m_ret = ExpNodeVisitor_Eval(&m_frame, node->left).getValue();
    }
    virtual void visit(StmtNode_Local *node) 
    {
        m_frame.declareLocal(node->name);
        if (!m_args.empty()) {
            m_frame.setValue(node->name, m_args[0]);
            m_args.erase(m_args.begin());
        }
    }
    virtual void visit(StmtNode_Block *node) 
    {
        m_frame.beginBlock();
        for (auto &stmt : node->stmts) {
            stmt->acceptVisitor(this);
            if (m_return || m_break || m_continue) break;
        }
        m_frame.endBlock();
    }
    virtual void visit(StmtNode_Exp *node) 
    {
        ExpNodeVisitor_Eval(&m_frame, node->exp);
    }
private:
    Value m_ret;
    vector<Value> m_args;
    StackFrame m_frame;
    bool m_continue, m_break, m_return;
};

Value ASTFunction::call(const vector<Value>& args)
{
    return StmtNodeVisitor_Execute(this, args).getRet();
}

static Value buildin_print(const vector<Value>& args)
{
    for (auto &v : args) cout << v.toString() << '\t';
    return Value();
}
static Value buildin_println(const vector<Value>& args)
{
    buildin_print(args);
    cout << endl;
    return Value();
}
static Value buildin_clock(const vector<Value>& args)
{
    return Value::createInt(clock());
}
static void registerBuildin()
{
    GlobalEnvironment *g = GlobalEnvironment::instance();
    g->registerFunc("print", FunctionPtr(new CFunction(buildin_print)));
    g->registerFunc("println", FunctionPtr(new CFunction(buildin_println)));
    g->registerFunc("clock", FunctionPtr(new CFunction(buildin_clock)));
}
static void convertToLua(const string& fname)
{
    GlobalEnvironment *g = GlobalEnvironment::instance();
    ofstream fo(fname.c_str());
    for (auto &name : g->getFuncNames()) {
        if (auto f = dynamic_cast<ASTFunction*>(g->getFunc(name).get())) {
            fo << StmtNodeVisitor_LuaCodeGen(name, f).getString();
        }
    }
    fo << "function print(...) for _, v in ipairs({...}) do io.write(v, '\\t') end end\n";
    fo << "function println(...) print(...); io.write('\\n') end\n";
    fo << "function clock() return os.clock() end\n";
    fo << "main()";
}

void parseFile(const char *fname);

int main(int argc, char *argv[])
{
    if (argc == 1) {
        puts("usage : interpreter file1 [file2 ...]");
        return 0;
    }

    try
    {
        registerBuildin();
        for (int i = 1; i < argc; ++i) parseFile(argv[i]);
        convertToLua("convert.lua");
        GlobalEnvironment::instance()->getFunc("main")->call(vector<Value>());
    }
    catch (const exception& e) {
        cout << "Exception : " << e.what() << endl;
    }
}
