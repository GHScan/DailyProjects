
#ifndef LUACODE_GEN
#define LUACODE_GEN

#include "AST.h"
#include "Runtime.h"

class ExpNodeVisitor_LuaCodeGen:
    public IExpNodeVisitor
{
public:
    ExpNodeVisitor_LuaCodeGen(const ExpNodePtr &e, ostream& so, int tab):
        m_so(so), m_tab(tab)
    {
        e->acceptVisitor(this);
        if (!m_str.empty() && m_str.front() == '(' && m_str.back() == ')') {
            m_str = m_str.substr(1, m_str.size() - 2);
        }
    }
    const string &getString() const { return m_str; }
private:
    virtual void visit(ExpNode_Assign *v)
    {
        string s = ExpNodeVisitor_LuaCodeGen(v->getRightMost()->right, m_so, m_tab).getString();
        m_so << tabString(m_tab) << v->name << " = " << s << endl;
        ExpNode_Assign *p = v;
        while (p = dynamic_cast<ExpNode_Assign*>(p->right.get())) {
            m_so << tabString(m_tab) << p->name << " = " << v->name << endl;
        }
        // m_str = v->name;
        m_str = "";
    }
    virtual void visit(ExpNode_BinaryOp *v)
    {
        v->left->acceptVisitor(this);
        string ls = m_str;
        v->right->acceptVisitor(this);
        string rs = m_str;
        string op = v->op;
        if (op == "&&") op = "and";
        else if (op == "||") op = "or";
        else if (op == "!=") op = "~=";
        else {}
        m_str = format("(%s %s %s)", ls.c_str(), op.c_str(), rs.c_str());
    }
    virtual void visit(ExpNode_UnaryOp *v)
    {
        v->left->acceptVisitor(this);
        m_str = v->op + m_str;
    }
    virtual void visit(ExpNode_Constant *v)
    {
        m_str = v->val.toReprString();
    }
    virtual void visit(ExpNode_Variable *v)
    {
        m_str = v->name;
    }
    virtual void visit(ExpNode_Call *v)
    {
        string s = v->name + "(";
        if (!v->params.empty()) {
            v->params[0]->acceptVisitor(this);
            s += m_str;
            for (int i = 1; i < (int)v->params.size(); ++i) {
                v->params[i]->acceptVisitor(this);
                s += ", " + m_str;
            }
        }
        s += ")";
        m_str = s;
    }
private:
    string m_str;
    ostream &m_so;
    int m_tab;
};

class StmtNodeVisitor_LuaCodeGen:
    public IStmtNodeVisitor
{
public:
    StmtNodeVisitor_LuaCodeGen(const string& fname, const ASTFunction* func):
        m_tab(0), m_specBlock(false)
    {
        auto block = static_cast<StmtNode_Block*>(func->stmt.get());
        m_so << format("function %s(", fname.c_str());
        if (func->argc > 0) {
            auto p = static_cast<StmtNode_Local*>(block->stmts[0].get());
            m_so << p->name;
            for (int i = 1; i < func->argc; ++i) {
                m_so << ", " << static_cast<StmtNode_Local*>(block->stmts[i].get())->name;
            }
        }
        m_so << ")\n";
        ++m_tab;
        for (int i = func->argc; i < (int)block->stmts.size(); ++i) {
            block->stmts[i]->acceptVisitor(this);
        }
        --m_tab;
        m_so << "end\n";
    }
    string getString() const { return m_so.str(); }
private:
    virtual void visit(StmtNode_IfElse *node)
    {
        string cond = ExpNodeVisitor_LuaCodeGen(node->condition, m_so, m_tab).getString();
        m_so << tabString(m_tab) << format("if %s then\n", cond.c_str());
        visitSpecStmtNode(node->ifStmt);
        if (node->elseStmt) {
            m_so << tabString(m_tab) << "else\n";
            visitSpecStmtNode(node->elseStmt);
        }
        m_so << tabString(m_tab) << "end\n";
    }
    virtual void visit(StmtNode_For *node)
    {
        string s;
        if (node->s1) {
            s = ExpNodeVisitor_LuaCodeGen(node->s1, m_so, m_tab).getString();
            if (!s.empty()) m_so << tabString(m_tab) << s << endl;
        }
        if (node->s2) {
            s = ExpNodeVisitor_LuaCodeGen(node->s2, m_so, m_tab).getString();
        }
        else s = "true";
        m_so << tabString(m_tab) << format("while %s do\n", s.c_str());
        visitSpecStmtNode(node->stmt);
        if (node->s3) {
            ++m_tab;
            s = ExpNodeVisitor_LuaCodeGen(node->s3, m_so, m_tab).getString();
            if (!s.empty()) m_so << tabString(m_tab) << s << endl;
            --m_tab;
        }
        m_so << tabString(m_tab) << "end\n";
    }
    virtual void visit(StmtNode_Continue *node)
    {
        ASSERT1(0, "lua is not support continue!");
    }
    virtual void visit(StmtNode_Break *node)
    {
        m_so << tabString(m_tab) << "break\n";
    }
    virtual void visit(StmtNode_Return *node)
    {
        string exp = ExpNodeVisitor_LuaCodeGen(node->left, m_so, m_tab).getString();
        m_so << tabString(m_tab) << format("return %s\n", exp.c_str());
    }
    virtual void visit(StmtNode_Local *node)
    {
        m_so << tabString(m_tab) << format("local %s\n", node->name.c_str());
    }
    virtual void visit(StmtNode_Block *node)
    {
        bool spec = m_specBlock;
        m_specBlock = false;
        if (!spec) m_so << tabString(m_tab) + "do\n";
        ++m_tab;
        for (auto &stmt : node->stmts) {
            stmt->acceptVisitor(this);
        }
        --m_tab;
        if (!spec) m_so << tabString(m_tab) + "end\n";
    }
    virtual void visit(StmtNode_Exp *node)
    {
        string exp = ExpNodeVisitor_LuaCodeGen(node->exp, m_so, m_tab).getString();
        if (!exp.empty()) m_so << tabString(m_tab) << exp << endl;
    }
private:
    void visitSpecStmtNode(const StmtNodePtr& stmt)
    {
        if (!stmt) return;
        if (auto p = dynamic_cast<StmtNode_Block*>(stmt.get())) { 
            m_specBlock = true;
            stmt->acceptVisitor(this);
        }
        else {
            ++m_tab;
            stmt->acceptVisitor(this);
            --m_tab;
        }
    }
private:
    bool m_specBlock;
    int m_tab;
    ostringstream m_so;
};

#endif
