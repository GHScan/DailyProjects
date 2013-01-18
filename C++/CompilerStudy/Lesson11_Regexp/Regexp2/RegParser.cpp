#include "pch.h"

#include <sstream>

#include "RegParser.h"

const char *REG_SPECIAL_CHAR = "\\{}[]().?*+-|";

//====================
// RegParserImpl
//====================

/*
    expr -> or
    or -> or '|' concat | concat
    concat -> concat repeat | repeat
    repeat -> unit? optmark | unit* optmark | unit+ optmark | unit{n} optmark | unit{m,n} optmark | unit
    optmark -> ? | e
    unit -> charset | capture
    capture -> (expr)
    charset -> [^ranges] | [ranges] | range
    ranges -> range ranges | range
    range -> single-single | single | buildincharset
    buildincharset -> \c | .
    single -> [^(){}\[]] | \c
*/

class RegParserImpl
{
public:
    RegParserImpl(const std::string& src):
        m_src(src), m_curToken(0)
    {
        m_node = sa_expr();
    }
    RegNodePtr& getNode() { return m_node; }
private:
    RegNodePtr sa_expr()
    {
        return sa_or();
    }
    RegNodePtr sa_or()
    {
        if (RegNodePtr left = sa_concat()) {
            return _sa_or(left);
        }
        else return RegNodePtr();
    }
    RegNodePtr _sa_or(const RegNodePtr& left)
    {
        if (!tryConsume('|')) return left;
        RegNodePtr right = sa_concat();
        assert(right);
        return _sa_or(RegNodePtr(new RegNode_Or(left, right)));
    }
    RegNodePtr sa_concat()
    {
        if (RegNodePtr left = sa_repeat()) {
            return _sa_concat(left);
        }
        else return RegNodePtr();
    }
    RegNodePtr _sa_concat(const RegNodePtr& left)
    {
        if (RegNodePtr right = sa_repeat()) {
            return _sa_concat(RegNodePtr(new RegNode_Concat(left, right)));
        }
        else return left;
    }
    RegNodePtr sa_repeat()
    {
        RegNodePtr r = sa_unit();
        if (tryConsume('?')) {
            RegNode_Repeat *repeat = new RegNode_Repeat(0, 1, true, r);
            r.reset(repeat);
        }
        else if (tryConsume('*')) {
            RegNode_Repeat *repeat = new RegNode_Repeat(0, REPEAT_MAX, true, r);
            r.reset(repeat);
        }
        else if (tryConsume('+')) {
            RegNode_Repeat *repeat = new RegNode_Repeat(1, REPEAT_MAX, true, r);
            r.reset(repeat);
        }
        else if (tryConsume('{')) {
            int min = 0, max = 0;
            assert(sa_int(min));
            max = min;
            if (tryConsume(',')) {
                sa_int(max);
            }
            consumeToken('}');
            RegNode_Repeat *repeat = new RegNode_Repeat(min, max, true, r);
            r.reset(repeat);
        }
        else ;
        if (r && tryConsume('?')) { 
            static_cast<RegNode_Repeat*>(r.get())->gready = false;
        }
        return r;
    }
    RegNodePtr sa_unit()
    {
        RegNodePtr r(sa_capture());
        if (!r) {
            r = sa_charset();
        }
        return r;
    }
    RegNodePtr sa_capture()
    {
        if (tryConsume('(')) {
            RegNode_Capture *cap = new RegNode_Capture(sa_expr());
            RegNodePtr r(cap);
            consumeToken(')');
            return r;
        }
        return RegNodePtr();
    }
    RegNodePtr sa_charset()
    {
        RegNode_Charset *p = new RegNode_Charset();
        RegNodePtr r(p);
        if (tryConsume('[')) {
            bool inv = tryConsume('^');
            sa_ranges(p->sets);
            if (inv) {
                for (int i = 0; i < p->sets.size(); ++i) {
                    if (p->sets.test(i)) p->sets.reset(i);
                    else p->sets.set(i);
                }
            }
            consumeToken(']');
        }
        else {
            sa_range(p->sets);
            if (!p->sets.any()) return RegNodePtr();
        }
        return r;
    }
    void sa_ranges(std::bitset<128>& sets)
    {
        if (sa_range(sets)) {
            return sa_ranges(sets);
        }
    }
    bool sa_range(std::bitset<128>& sets)
    {
        char c;
        if (sa_buildincharset(c)) {
            switch (c) {
                case 'a':
                    for (int i = 0; i < sets.size(); ++i) if (isalpha(i)) sets.set(i);
                    break;
                case 'd':
                    for (int i = 0; i < sets.size(); ++i) if (isdigit(i)) sets.set(i);
                    break;
                case 's':
                    for (int i = 0; i < sets.size(); ++i) if (isspace(i)) sets.set(i);
                    break;
                case 'w':
                    for (int i = 0; i < sets.size(); ++i) if (isalpha(i) || isdigit(i)) sets.set(i);
                    break;
                case '.':
                    for (int i = 0; i < sets.size(); ++i) sets.set(i);
                    break;
                default:
                    assert(0);
                    return false;
            }
            return true;
        }
        else {
            if (sa_single(c)) {
                if (tryConsume('-')) {
                    char c2 = 0;
                    assert(sa_single(c2) && c <= c2);
                    for (int i = c; i <= c2; ++i) sets.set(i);
                }
                else sets.set(c);
                return true;
            }
            return false;
        }
    }
    bool sa_buildincharset(char &c)
    {
        if (tryConsume('.')) {
            c = '.';
            return true;
        }
        backupTokenPos();
        if (tryConsume('\\')) {
            switch (curToken()) {
                case 'a': case 'd': case 'w': case 's':
                    discardTokenPos();
                    c = curToken();
                    ++m_curToken;
                    return true;
                default:
                    break;
            }
        }
        restoreTokenPos();
        return false;
    }
    bool sa_single(char &c)
    {
        if (tryConsume('\\')) {
            assert(hasMoreToken());
            c = curToken();
            ++m_curToken;
            return true;
        }
        else {
            if (!hasMoreToken()) return false;
            if (!strchr(REG_SPECIAL_CHAR, curToken())) {
                c = curToken();
                ++m_curToken;
                return true;
            }
        }
        return false;
    }
    bool sa_int(int &r)
    {
        if (!isdigit(curToken())) return false;
        r = 0;
        while (isdigit(curToken())) {
            r = r * 10 + (curToken() - '0');
            ++m_curToken;
        }
        return true;
    }
private:
    bool tryConsume(char c) 
    {
        if (hasMoreToken() && m_src[m_curToken] == c) {
            ++m_curToken;
            return true;
        }
        return false;
    }
    void consumeToken(char c) { assert(tryConsume(c)); }
    bool hasMoreToken() const { return m_curToken < m_src.size();}
    char curToken() const { assert(hasMoreToken()); return m_src[m_curToken]; }
    char preToken() const { return m_src[m_curToken - 1]; }
    void backupTokenPos(){ m_backupTokenPos.push_back(m_curToken);}
    void restoreTokenPos() {m_curToken = m_backupTokenPos.back(); m_backupTokenPos.pop_back();}
    void discardTokenPos(){ m_backupTokenPos.pop_back();}

private:
    std::string m_src;
    int m_curToken;
    RegNodePtr m_node;
    std::vector<int> m_backupTokenPos;
};

RegParser::RegParser(const std::string& src):
    m_impl(new RegParserImpl(src))
{ }
RegParser::~RegParser()
{
    delete m_impl;
}
RegNodePtr& RegParser::getNode()
{
    return m_impl->getNode();
}
//====================

static void printChar(std::ostringstream& so, char c)
{
    if (strchr(REG_SPECIAL_CHAR, c)) so << '\\';
    so << c;
}
static void printCharRange(std::ostringstream& so, const std::bitset<128>& sets)
{
    so << '[';
    char c = -1;
    for (int i = 0; i <= sets.size(); ++i) {
        if (i < sets.size() && sets.test(i)) {
            if (c == -1) c = i;
        }
        else {
            if (c != -1) {
                if (i == c + 1) printChar(so, c);
                else if (i == c + 2) printChar(so, c), printChar(so, c + 1);
                else {
                    printChar(so, c);
                    so << '-';
                    printChar(so, i - 1);
                }
                c = -1;
            }
        }
    }
    so << ']';
}
//====================
// RegNodeVisitor_PrinterImpl
//====================
class RegNodeVisitor_PrinterImpl:
    public IRegNodeVisitor
{
public:
    std::string apply(RegNodePtr node)
    {
        m_so.str("");
        node->acceptVisitor(this);
        return m_so.str();
    }

private:
    virtual void visit(RegNode_Charset *v)
    {
        int n = 0;
        char c;
        for (int i = 0; i < v->sets.size(); ++i) {
            if (v->sets.test(i)) {
                ++n;
                c = char(i);
            }
        }
        if (n == 1) {
            printChar(m_so, c);
        }
        else if (n == v->sets.size()) {
            m_so << '.';
        }
        else printCharRange(m_so, v->sets);
    }
    virtual void visit(RegNode_Capture *v)
    {
        m_so << '(';
        v->node->acceptVisitor(this);
        m_so << ')';
    }
    virtual void visit(RegNode_Repeat *v)
    {
        v->node->acceptVisitor(this);
        if (v->min == 0 && v->max == 1) m_so << '?';
        else if (v->min == 0 && v->max == REPEAT_MAX) m_so << '*';
        else if (v->min == 1 && v->max == REPEAT_MAX) m_so << '+';
        else {
            if (v->min == v->max) m_so << '{' << v->min << '}';
            else m_so << '{' << v->min << ',' << v->max << '}';
        }
    }
    virtual void visit(RegNode_Concat *v)
    {
        v->left->acceptVisitor(this);
        v->right->acceptVisitor(this);
    }
    virtual void visit(RegNode_Or *v)
    {
        v->left->acceptVisitor(this);
        m_so << "|";
        v->right->acceptVisitor(this);
    }
private:
    std::ostringstream m_so;
};

RegNodeVisitor_Printer::RegNodeVisitor_Printer():
    m_impl(new RegNodeVisitor_PrinterImpl())
{ }
RegNodeVisitor_Printer::~RegNodeVisitor_Printer()
{
    delete m_impl;
}
std::string RegNodeVisitor_Printer::apply(RegNodePtr node)
{
    return m_impl->apply(node);
}
//====================
// RegNodeVisitor_LogicPrinterImpl
//====================
class RegNodeVisitor_LogicPrinterImpl:
    public IRegNodeVisitor
{
public:
    std::string apply(const RegNodePtr& node) 
    {
        m_so.str("");
        node->acceptVisitor(this);
        return m_so.str();
    }
private:
    virtual void visit(RegNode_Charset *v)
    {
        printCharRange(m_so, v->sets);
    }
    virtual void visit(RegNode_Capture *v)
    {
        m_so << '(';
        v->node->acceptVisitor(this);
        m_so << ')';
    }
    virtual void visit(RegNode_Repeat *v)
    {
        v->node->acceptVisitor(this);
        m_so << '{' << v->min << ',' << v->max << '}';
        if (!v->gready) m_so << '?';
    }
    virtual void visit(RegNode_Concat *v)
    {
        v->left->acceptVisitor(this);
        v->right->acceptVisitor(this);
    }
    virtual void visit(RegNode_Or *v)
    {
        m_so << '(';
        v->left->acceptVisitor(this);
        m_so << ")|(";
        v->right->acceptVisitor(this);
        m_so << ')';
    }
private:
    void printChar(char c)
    {
        if (strchr(REG_SPECIAL_CHAR, c)) m_so << '\\';
        m_so << c;
    }
private:
    std::ostringstream m_so;
};

RegNodeVisitor_LogicPrinter::RegNodeVisitor_LogicPrinter():
    m_impl(new RegNodeVisitor_LogicPrinterImpl)
{ }
RegNodeVisitor_LogicPrinter::~RegNodeVisitor_LogicPrinter()
{
    delete m_impl;
}
std::string RegNodeVisitor_LogicPrinter::apply(RegNodePtr node)
{
    return m_impl->apply(node);
}
