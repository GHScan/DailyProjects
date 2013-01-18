
#include "pch.h" 

#include <assert.h>

#include <string>
#include <vector>
#include <memory>
/*
    expr = expr repeat | repeat
    repeat = factor * | factor + | factor ? | factor +? | factor *? | factor {d} | factor {d1,d2} | factor
    factor =  capture | include | backwardcapture | char 
    capture = (expr)
    include = [charlist] | [^charlist]
    charlist = char+
    backwardcapture = \d
    char = const | charset
    charset = \a | \d | \w | \s | \\
    const = [^\[]{}()]
 */

const int MAX_REPEAT = 100000;

struct ExprNode_Const;
struct ExprNode_Charset;
struct ExprNode_Include;
struct ExprNode_Capture;
struct ExprNode_BackwardCapture;
struct ExprNode_Expr;
struct ExprNode_Repeat;
struct IExprTreeVisitor
{
    virtual ~IExprTreeVisitor(){}
    virtual void visit(ExprNode_Const *node) = 0;
    virtual void visit(ExprNode_Charset *node) = 0;
    virtual void visit(ExprNode_Include *node) = 0;
    virtual void visit(ExprNode_Capture *node) = 0;
    virtual void visit(ExprNode_BackwardCapture *node) = 0;
    virtual void visit(ExprNode_Expr *node) = 0;
    virtual void visit(ExprNode_Repeat *node) = 0;
};

struct IExprNode
{
    virtual ~IExprNode(){}
    virtual void acceptVisitor(IExprTreeVisitor *v) = 0;
};
typedef std::shared_ptr<IExprNode> ExprNodePtr;
struct ExprNode_Const:
    public IExprNode
{
    char c;
    ExprNode_Const(char c){this->c = c;}
    virtual void acceptVisitor(IExprTreeVisitor *v){ v->visit(this);}
};
struct ExprNode_Charset:
    public IExprNode
{
    char c;
    ExprNode_Charset(char c){this->c = c;}
    virtual void acceptVisitor(IExprTreeVisitor *v){ v->visit(this);}
};
struct ExprNode_Include:
    public IExprNode
{
    std::vector<ExprNodePtr> chars;
    bool include;
    virtual void acceptVisitor(IExprTreeVisitor *v){ v->visit(this);}
};
struct ExprNode_Capture:
    public IExprNode
{
    ExprNodePtr expr;
    int n;
    ExprNode_Capture(int n) {this->n = n;}
    virtual void acceptVisitor(IExprTreeVisitor *v){ v->visit(this);}
};
struct ExprNode_BackwardCapture:
    public IExprNode
{
    int n;
    ExprNode_BackwardCapture(int n){this->n = n;}
    virtual void acceptVisitor(IExprTreeVisitor *v){ v->visit(this);}
};
struct ExprNode_Expr:
    public IExprNode
{
    std::vector<ExprNodePtr> list;
    virtual void acceptVisitor(IExprTreeVisitor *v){ v->visit(this);}
};
struct ExprNode_Repeat:
    public IExprNode
{
    ExprNodePtr factor;
    bool gready;
    int minT, maxT;
    void setInfo(bool gready, int minT, int maxT) 
    {
        this->gready = gready, this->minT = minT, this->maxT = maxT; 
    }
    virtual void acceptVisitor(IExprTreeVisitor *v){ v->visit(this);}
};

class Parser
{
public:
    ExprNodePtr parse(const std::string& src)
    {
        m_src = src;
        m_curToken = 0;
        m_nextCap = 1;
        return expr();
    }
private:
    ExprNodePtr expr()
    {
        ExprNode_Expr *p = new ExprNode_Expr();
        ExprNodePtr r(p);
        while (ExprNodePtr re = repeat()) {
            p->list.push_back(re);
        }
        if (p->list.size() == 1) return p->list[0];
        assert(p->list.size() > 0);
        return r;
    }
    ExprNodePtr repeat()
    {
        if (ExprNodePtr fac = factor()) {
            ExprNode_Repeat *repeat = new ExprNode_Repeat();
            ExprNodePtr r(repeat);

            backupPos();
            if (tryConsumeToken('*')) {
                discardBackup();
                repeat->factor = fac;
                repeat->setInfo(true, 0, MAX_REPEAT);
                return r;
            }
            restorePos();
            backupPos();
            if (tryConsumeToken('*') && tryConsumeToken('?')) {
                discardBackup();
                repeat->factor = fac;
                repeat->setInfo(false, 0, MAX_REPEAT);
                return r;
            }
            restorePos();
            backupPos();
            if (tryConsumeToken('+')) {
                discardBackup();
                repeat->factor = fac;
                repeat->setInfo(true, 1, MAX_REPEAT);
                return r;
            }
            restorePos();
            backupPos();
            if (tryConsumeToken('+') && tryConsumeToken('?')) {
                discardBackup();
                repeat->factor = fac;
                repeat->setInfo(false, 1, MAX_REPEAT);
                return r;
            }
            restorePos();
            backupPos();
            if (tryConsumeToken('?')) {
                discardBackup();
                repeat->factor = fac;
                repeat->setInfo(true, 0, 1);
                return r;
            }
            restorePos();
            backupPos();
            if (tryConsumeToken('{')) {
                discardBackup();
                int minT = number();
                int maxT = minT;
                if (tryConsumeToken(',')) {
                    maxT = number();
                }
                consumeToken('}');
                repeat->factor = fac;
                repeat->setInfo(true, minT, maxT);
                return r;
            }
            restorePos();
            return fac;
        }
        else return ExprNodePtr();
    }
    ExprNodePtr factor()
    {
        ExprNodePtr p = capture();
        if (p != NULL) return p;
        if (p = include()) return p;
        if (p = backwardcapture()) return p;
        return _char();
    }
    ExprNodePtr capture()
    {
        if (tryConsumeToken('(')) {
            ExprNode_Capture *c = new ExprNode_Capture(m_nextCap++);
            ExprNodePtr r(c);
            c->expr = expr();
            consumeToken(')');
            return r;
        }
        else return ExprNodePtr();
    }
    ExprNodePtr include()
    {
        if (tryConsumeToken('[')) {
            ExprNode_Include *i = new ExprNode_Include();
            ExprNodePtr r(i);
            i->include = true;
            if (tryConsumeToken('^')) {
                i->include = false;
            }
            while (ExprNodePtr c = _char()) {
                i->chars.push_back(c);
            }
            consumeToken(']');
            return r;
        }
        else return ExprNodePtr();
    }
    ExprNodePtr backwardcapture()
    {
        backupPos();
        if (tryConsumeToken('\\') && isdigit(curToken())) {
            discardBackup();
            consumeToken();
            return ExprNodePtr(new ExprNode_BackwardCapture(previewToken() - '0'));
        }
        else {
            restorePos();
            return ExprNodePtr();
        }
    }
    ExprNodePtr _char()
    {
        if (ExprNodePtr p = charset()) return p;
        return _const();
    }
    ExprNodePtr charset()
    {
        const char *sets = "wsad";
        backupPos();
        if (tryConsumeToken('\\') && strchr(sets, curToken())) {
            discardBackup();
            consumeToken();
            return ExprNodePtr(new ExprNode_Charset(previewToken()));
        }
        else {
            restorePos();
            return ExprNodePtr();
        }
    }
    ExprNodePtr _const()
    {
        if (tryConsumeToken('\\')) {
            consumeToken();
            return ExprNodePtr(new ExprNode_Const(previewToken()));
        }
        else {
            const char* invalidStr = "{}\\()[]";
            if (!strchr(invalidStr, curToken()) && tryConsumeToken()) {
                return ExprNodePtr(new ExprNode_Const(previewToken()));
            }
        }
        return ExprNodePtr();
    }
    int number()
    {
        int r = 0;
        while (hasMoreToken() && isdigit(curToken())) {
            r = r * 10 + (curToken() - '0');
            ++m_curToken;
        }
        return r;
    }
private:
    char curToken() const { return m_src[m_curToken]; }
    char previewToken() const { return m_src[m_curToken - 1]; }
    bool hasMoreToken() const { return m_curToken < m_src.size(); }
    bool tryConsumeToken(char c) 
    {
        if (hasMoreToken() && c == curToken()) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    bool tryConsumeToken()
    {
        if (hasMoreToken()) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    void consumeToken(char c) { assert(tryConsumeToken(c)); }
    void consumeToken() { assert(tryConsumeToken()); }
    void backupPos() { m_backupTokenPos.push_back(m_curToken); }
    void restorePos() { m_curToken = m_backupTokenPos.back(); m_backupTokenPos.pop_back();}
    void discardBackup() { m_backupTokenPos.pop_back(); }

private:
    int m_nextCap;
    std::string m_src;
    int m_curToken;
    std::vector<int> m_backupTokenPos;
};

class ExprTreeVisitor_Printer :
    public IExprTreeVisitor
{
public:
    void apply(ExprNodePtr p)
    {
        p->acceptVisitor(this);
        cout << endl;
    }
private:
    virtual void visit(ExprNode_Const *node)
    {
        cout << node->c;
    }
    virtual void visit(ExprNode_Charset *node)
    {
        cout << '\\' << node->c;
    }
    virtual void visit(ExprNode_Include *node)
    {
        cout << '[';
        if (!node->include) cout << '^';
        for (int i = 0; i < node->chars.size(); ++i) {
            node->chars[i]->acceptVisitor(this);
        }
        cout << ']';
    }
    virtual void visit(ExprNode_Capture *node)
    {
        cout << '(';
        cout << node->n;
        node->expr->acceptVisitor(this);
        cout << ')';
    }
    virtual void visit(ExprNode_BackwardCapture *node) 
    {
        cout << '\\' << node->n;
    }
    virtual void visit(ExprNode_Expr *node) 
    {
        for (int i = 0; i < node->list.size(); ++i) {
            node->list[i]->acceptVisitor(this);
        }
    }
    virtual void visit(ExprNode_Repeat *node)
    {
        node->factor->acceptVisitor(this);
        cout << '{';
        cout << node->minT << ',' << node->maxT;
        cout << '}';
        if (!node->gready) cout << '?';
    }
};

int main()
{
    Parser p;
    ExprTreeVisitor_Printer v;
    v.apply(p.parse("abcd"));
    v.apply(p.parse("ab+c?d{2}"));
    v.apply(p.parse("[_azAZ]\\w+"));
    v.apply(p.parse("([_azAZ]\\w+){2}\\s\\1"));
}
