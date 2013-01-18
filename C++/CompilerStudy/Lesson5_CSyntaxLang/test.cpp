#include "pch.h" 

#include <fstream>

#include <time.h>

#include "common.h"
#include "parser.h"

//==============================
// runtime
//==============================
typedef std::unordered_map<std::string, ValuePtr> Context;
typedef std::vector<Context> StackFrameContext;
struct GlobalEnvironment;

struct IFunction
{
    virtual ~IFunction() {}
    virtual ValuePtr call(GlobalEnvironment& g, const std::vector<ValuePtr>& args) = 0;
};
typedef std::shared_ptr<IFunction> FunctionPtr;

struct GlobalEnvironment
{
    Context globalVars;
    std::vector<StackFrameContext> stackVars;
    std::unordered_map<std::string, FunctionPtr> funcs;

    void beginFunction()
    {
        stackVars.push_back(StackFrameContext());
        stackVars.back().push_back(Context());
    }
    void endFunction()
    {
        stackVars.back().pop_back();
        ASSERT(stackVars.back().empty());
        stackVars.pop_back();
    }
    void beginBlock()
    {
        stackVars.back().push_back(Context());
    }
    void endBlock()
    {
        stackVars.back().pop_back();
    }
    ValuePtr getGlobal(const std::string& name)
    {
        return globalVars[name];
    }
    void setGlobal(const std::string& name, ValuePtr v) 
    {
        globalVars[name] = v;
    }
    void declareLocal(const std::string& name, ValuePtr v)
    {
        Context& c = stackVars.back().back();
        c[name] = v;
    }
    ValuePtr getLocal(const std::string& name)
    {
        StackFrameContext& f = stackVars.back();
        for (int i = f.size() - 1; i >= 0; --i) {
            if (f[i].count(name)) return f[i][name];
        }
        return ValuePtr();
    }
    bool setLocal(const std::string& name, ValuePtr v)
    {
        StackFrameContext& f = stackVars.back();
        for (int i = f.size() - 1; i >= 0; --i) {
            if (f[i].count(name)) {
                f[i][name] = v;
                return true;
            }
        }
        return false;
    }
    ValuePtr getValue(const std::string& name)
    {
        ValuePtr r = getLocal(name);
        if (r != NULL) return r;
        return getGlobal(name);
    }
    void setValue(const std::string& name, ValuePtr v)
    {
        if (!setLocal(name, v)) setGlobal(name, v);
    }
};

class CFunction:
    public IFunction
{
public:
    typedef ValuePtr(*FuncT) (GlobalEnvironment& g, const std::vector<ValuePtr>& args);
    FuncT m_f;
    CFunction(FuncT f): m_f(f){}
    virtual ValuePtr call(GlobalEnvironment& g, const std::vector<ValuePtr>& args)
    {
        return m_f(g, args);
    }
};

class SyntaxTreeVisitor_FuncVisitor:
    public ISyntaxTreeVisitor
{
public:
    SyntaxTreeVisitor_FuncVisitor(
        FuncSyntaxPtr funcNode, 
        GlobalEnvironment& g, const std::vector<ValuePtr>& args):
        m_g(g), m_args(args), m_isRet(false), m_isBreak(false), m_loopLevel(0), m_isContinue(false)
    {
        m_g.beginFunction();
        funcNode->args->acceptVisitor(this);
        funcNode->stmts->acceptVisitor(this);
    }
    ~SyntaxTreeVisitor_FuncVisitor()
    {
        m_g.endFunction();
    }
    ValuePtr getResult()
    {
        return m_ret;
    }

private:
    virtual void visit(ExprNode_Const *node)
    {
        m_valueStack.push_back(ValuePtr(node->value->clone()));
    }
    virtual void visit(ExprNode_ID *node)
    {
        m_valueStack.push_back(ValuePtr(m_g.getValue(node->name)->clone()));
    }
    virtual void visit(ExprNode_Call *node)
    {
        std::vector<ValuePtr> args;
        for (int i = 0; i < node->params.size(); ++i) {
            args.push_back(eval(node->params[i]));
        }
        m_valueStack.push_back(m_g.funcs[node->fname]->call(m_g, args));
    }
    virtual void visit(ExprNode_UnaryOp *node)
    {
        ValuePtr r = eval(node->child);
        if (node->op == "!") {
            r.reset(new NumValue(!r->toBoolean() ? 1 : 0));
            m_valueStack.push_back(r);
        }
        else ASSERT(0);
    }
    virtual void visit(ExprNode_BinaryOp *node)
    {
        ValuePtr lval = eval(node->left);
        if (node->op == "&&") {
            ValuePtr v(new NumValue(lval->toBoolean() && 
                        eval(node->right)->toBoolean() ? 1 : 0));
            m_valueStack.push_back(v);
            return;
        }
        else if (node->op == "||") {
            ValuePtr v(new NumValue(lval->toBoolean() || 
                        eval(node->right)->toBoolean() ? 1 : 0));
            m_valueStack.push_back(v);
            return;
        }

        ValuePtr rval = eval(node->right);
        if (node->op == "+") {
            lval->add(rval.get());
            m_valueStack.push_back(lval);
        }
        else if (node->op == "-") {
            lval->sub(rval.get());
            m_valueStack.push_back(lval);
        }
        else if (node->op == "*") {
            lval->mul(rval.get());
            m_valueStack.push_back(lval);
        }
        else if (node->op == "/") {
            lval->div(rval.get());
            m_valueStack.push_back(lval);
        }
        else if (node->op == "%") {
            lval->mod(rval.get());
            m_valueStack.push_back(lval);
        }
        else if (node->op == "<") {
            ValuePtr v(new NumValue(lval->less(rval.get()) ? 1 : 0));
            m_valueStack.push_back(v);
        }
        else if (node->op == "<=") {
            ValuePtr v(new NumValue(lval->lessEqual(rval.get()) ? 1 : 0));
            m_valueStack.push_back(v);
        }
        else if (node->op == ">") {
            ValuePtr v(new NumValue(lval->greater(rval.get()) ? 1 : 0));
            m_valueStack.push_back(v);
        }
        else if (node->op == ">=") {
            ValuePtr v(new NumValue(lval->greaterEqual(rval.get()) ? 1 : 0));
            m_valueStack.push_back(v);
        }
        else if (node->op == "==") {
            ValuePtr v(new NumValue(lval->equal(rval.get()) ? 1 : 0));
            m_valueStack.push_back(v);
        }
        else if (node->op == "!=") {
            ValuePtr v(new NumValue(lval->equal(rval.get()) ? 0 : 1));
            m_valueStack.push_back(v);
        }
        else ASSERT(0);
    }
    virtual void visit(ExprNode_Assign *node)
    {
        ValuePtr r = eval(node->right);
        m_g.setValue(node->left, r);
        m_valueStack.push_back(r);
    }
    virtual void visit(StmtNode_ExprOnly *node)
    {
        m_valueStack.clear();
        if (node->expr != NULL) eval(node->expr);
    }
    virtual void visit(StmtNode_IfElse *node)
    {
        m_valueStack.clear();
        if (eval(node->expr)->toBoolean()) {
            node->ifStmt->acceptVisitor(this);
        }
        else {
            if (node->elseStmt != NULL) node->elseStmt->acceptVisitor(this);
        }
    }
    virtual void visit(StmtNode_While *node)
    {
        m_valueStack.clear();
        ++m_loopLevel;
        while (!m_isRet && !m_isBreak) {
            ValuePtr v = eval(node->expr);
            if (!v->toBoolean()) break;
            m_isContinue = false;
            node->stmt->acceptVisitor(this);
        }
        --m_loopLevel;
        if (m_isBreak) m_isBreak = false;
    }
    virtual void visit(StmtNode_Stmts *node)
    {
        m_valueStack.clear();
        for (int i = 0; i < node->stmts.size() && !m_isRet && !m_isContinue; ++i) {
            node->stmts[i]->acceptVisitor(this);
        }
    }
    virtual void visit(StmtNode_Local *node)
    {
        m_valueStack.clear();
        ValuePtr v;
        if (node->right != NULL) {
            v = eval(node->right);
        }
        else {
            if (!m_args.empty()) {
                v = m_args[0];
                m_args.erase(m_args.begin());
            }
        }
        m_g.declareLocal(node->left, v);
    }
    virtual void visit(StmtNode_Return *node)
    {
        m_valueStack.clear();
        m_isRet = true;
        if (node->right != NULL) {
            m_ret = eval(node->right);
        }
    }
    virtual void visit(StmtNode_Break *node)
    {
        m_valueStack.clear();
        m_isBreak = true;
    }
    virtual void visit(StmtNode_Continue *node)
    {
        m_valueStack.clear();
        m_isContinue = true;
    }
private:
    ValuePtr eval(ExprNodePtr p)
    {
        ValuePtr r;
        p->acceptVisitor(this);
        r = m_valueStack.back();
        m_valueStack.pop_back();
        return r;
    }
private:
    std::vector<ValuePtr> m_valueStack;
    GlobalEnvironment &m_g;
    ValuePtr m_ret;
    std::vector<ValuePtr> m_args;
    int m_loopLevel;
    bool m_isRet;
    bool m_isBreak;
    bool m_isContinue;
};

class ASTFunction:
    public IFunction
{
public:
    ASTFunction(FuncSyntaxPtr func): m_func(func){}
    virtual ValuePtr call(GlobalEnvironment& g, const std::vector<ValuePtr>& args)
    {
        return SyntaxTreeVisitor_FuncVisitor(m_func, g, args).getResult();
    }
private:
    FuncSyntaxPtr m_func;
};

ValuePtr buildin_print(GlobalEnvironment& g, const std::vector<ValuePtr>& args)
{
    for (int i = 0; i < args.size(); ++i) cout << args[i]->toString() << '\t';
    return ValuePtr();
}
ValuePtr buildin_println(GlobalEnvironment& g, const std::vector<ValuePtr>& args)
{
    ValuePtr r = buildin_print(g, args);
    cout << endl;
    return r;
}
ValuePtr buildin_clock(GlobalEnvironment& g, const std::vector<ValuePtr>& args)
{
    return ValuePtr(new NumValue(clock()));
}

std::string readFile(const char *fname)
{
    std::fstream fi(fname);
    std::string r;
    for (std::string line; getline(fi, line); r += line + '\n');
    return r;
}
void parseFile(const char *fname, GlobalEnvironment& g)
{
    Parser parser;
    parser.parse(fname, readFile(fname));
    for (int i = 0; i < parser.getFuncs().size(); ++i) {
        const std::string fname = ((const FuncSyntax*)parser.getFuncs()[i].get())->name;
        g.funcs[fname] = FunctionPtr(new ASTFunction(parser.getFuncs()[i]));
    }
}
void registerBuildinFunctions(GlobalEnvironment& g)
{
    g.funcs["print"] = FunctionPtr(new CFunction(&buildin_print));
    g.funcs["println"] = FunctionPtr(new CFunction(&buildin_println));
    g.funcs["clock"] = FunctionPtr(new CFunction(&buildin_clock));
}
void runMain(GlobalEnvironment& g)
{
    g.funcs["main"]->call(g, std::vector<ValuePtr>());
}

int main()
{
    try{
        GlobalEnvironment g;
        parseFile("1.txt", g);
        registerBuildinFunctions(g);
        runMain(g);
    }
    catch (const std::exception& e) {
        cout << "Exception: " << e.what() << endl;
    }
}
