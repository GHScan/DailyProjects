
#include "pch.h"
#include "RegParser.h"
#include "DynamicBitset.h"

/*
    Reg : Or;
    Or : Concat '|' Or | Concat;
    Concat : Repeat | Repeat Concat;
    Repeat : Term '+' | Term '*' | Term '?' | Term;
    Term : Capture | RangeCharset | SpecialCharset | Char;
    Char : .;
    Capture : '(' Reg ')';
    RCharSeq : ([^']]|'.')+;
    RangeCharset : '[' RCharSeq ']' | '[' '^' RCharSeq ']';
    SpecialCharset : '\' ([adwsADWStn] | .) ;
*/

class RegParserImpl
{
public:
    RegNodePtr parse(const string& src)
    {
        m_src = src, m_curPos = 0;
        return _reg();
    }
private:
    RegNodePtr _reg()
    {
        return _or();
    }
    RegNodePtr _or()
    {
        auto r = _concat();
        if (tryConsume('|')) {
            r.reset(new RegNode_Or(r, _or()));
        }
        return r;
    }
    RegNodePtr _concat()
    {
        auto r = _repeat();
        if (!r) return r;
        if (auto n = _concat()) {
            r.reset(new RegNode_Concat(r, n));
        }
        return r;
    }
    RegNodePtr _repeat()
    {
        auto r = _term();
        if (tryConsume('+')) {
            r.reset(new RegNode_Repeat(r, 1, RegNode_Repeat::MAX_REPEAT));
        }
        else if (tryConsume('*')) {
            r.reset(new RegNode_Repeat(r, 0, RegNode_Repeat::MAX_REPEAT));
        }
        else if (tryConsume('?')) {
            r.reset(new RegNode_Repeat(r, 0, 1));
        }
        else {}
        return r;
    }
    RegNodePtr _term()
    {
        if (auto r = _capture()) return r;
        else if (auto r = _rangeCharset()) return r;
        else {
            auto p = new RegNode_Charset;
            RegNodePtr r2(p);
            if (_specialCharset(p) || _char(p)) {
                return r2;
            }
            else return RegNodePtr();
        }
    }
    RegNodePtr _capture()
    {
        if (tryConsume('(')) {
            RegNodePtr r(new RegNode_Capture(_reg()));
            consume(')');
            return r;
        }
        return RegNodePtr();
    }
    void _rangeCharSeq(RegNode_Charset* node)
    {
        for (;;) {
            if (_specialCharset(node) || _char(node)) continue;
            if (cur() == '-') {
                char first = pre();
                consume();
                char last = cur();
                ASSERT(_char(node));
                while (++first < last) node->chars[first] = true;
            }
            else break;
        }
    }
    RegNodePtr _rangeCharset()
    {
        if (tryConsume('[')) {
            auto p = new RegNode_Charset();
            RegNodePtr r(p);
            bool isInv = tryConsume('^');
            _rangeCharSeq(p);
            if (isInv) p->inverse();
            consume(']');
            return r;
        }
        return RegNodePtr();
    }
    bool _specialCharset(RegNode_Charset* node)
    {
        if (tryConsume('.')) {
            for (auto &b : node->chars) b = true;
            return true;
        }
        else if (tryConsume('\\')) {
            DynamicBitset set(256);
            switch (cur()) {
                case 'a': case 'A':
                    for (int i = 0; i < set.size(); ++i) if (isalpha(i)) set.set(i);
                    if (isupper(cur())) set.inverse();
                    break;
                case 'd': case 'D':
                    for (int i = 0; i < set.size(); ++i) if (isdigit(i)) set.set(i);
                    if (isupper(cur())) set.inverse();
                    break;
                case 's': case 'S':
                    for (int i = 0; i < set.size(); ++i) if (isspace(i)) set.set(i);
                    if (isupper(cur())) set.inverse();
                    break;
                case 'w': case 'W':
                    for (int i = 0; i < set.size(); ++i) if (isalnum(i)) set.set(i);
                    if (isupper(cur())) set.inverse();
                    break;
                case 't':
                    set.set('\t');
                    break;
                case 'n':
                    set.set('\n');
                    break;
                case 'r':
                    set.set('\r');
                    break;
                default:
                    set.set(cur());
                    break;
            }
            for (auto i : set.toInts()) node->chars[i] = true;
            consume();
            return true;
        }
        return false;
    }
    bool _char(RegNode_Charset *node)
    {
        if (strchr(".\\[]{}()|?+*-", cur()) == 0) {
            node->chars[cur()] = true;
            consume();
            return true;
        }
        return false;
    }
private:
    bool tryConsume(char c)
    {
        if (m_src[m_curPos] == c) {
            ++m_curPos;
            return true;
        }
        return false;
    }
    void consume() { ++m_curPos; }
    void consume(char c) { ASSERT(tryConsume(c)); }
    char cur() const { return m_src[m_curPos];}
    char pre() const { return m_src[m_curPos - 1]; }
private:
    string m_src;
    int m_curPos;
};

RegParser::RegParser(const string& src)
{
    m_root = RegParserImpl().parse(src);
}
// RegNodeVisitor_Printer
string RegNodeVisitor_Printer::apply(const RegNodePtr node)
{
    node->acceptVisitor(this);
    return m_str;
}
static string char2String(int c)
{
    switch (c) { 
        case '\t': return "\\t";
        case '\n': return "\\n";
        case '\r': return "\\r";
        default:
            if (isgraph(c)) return format("%c", c);
            else return format("\\%02x", c);
    }
}
void RegNodeVisitor_Printer::visit(const RegNode_Charset* node)
{
    if (node->count() == 1) {
        int c = 0;
        for (int i = 0; i < COUNT_OF(node->chars); ++i) {
            if (node->chars[i]) {
                c = i;
                break;
            }
        }
        m_str = char2String(c);
    }
    else {
        m_str = "[";
        int start = -1;
        for (int i = 0; i < COUNT_OF(node->chars) + 1; ++i) {
            if (i < COUNT_OF(node->chars) && node->chars[i]) {
                if (start == -1) {
                    start = i;
                    m_str += char2String(i);
                }
            }
            else {
                if (start != -1) {
                    if (i - 1 > start + 1) {
                        m_str += char2String('-');
                        m_str += char2String(i - 1);
                    }
                    start = -1;
                }
            }
        }
        m_str += "]";
    }
}
void RegNodeVisitor_Printer::visit(const RegNode_Concat* node)
{
    node->left->acceptVisitor(this);
    string ls = m_str;
    node->right->acceptVisitor(this);
    string rs = m_str;
    m_str = format("%s%s", ls.c_str(), rs.c_str());
}
void RegNodeVisitor_Printer::visit(const RegNode_Or* node)
{
    node->left->acceptVisitor(this);
    string ls = m_str;
    node->right->acceptVisitor(this);
    string rs = m_str;
    m_str = format("%s|%s", ls.c_str(), rs.c_str());
}
void RegNodeVisitor_Printer::visit(const RegNode_Repeat* node)
{
    node->node->acceptVisitor(this);
    const char *sym = "";
    if (node->min == 0) {
        if (node->max == 1) sym = "?";
        else {
            ASSERT(node->max == RegNode_Repeat::MAX_REPEAT);
            sym = "*";
        }
    }
    else {
        ASSERT(node->max == RegNode_Repeat::MAX_REPEAT);
        sym = "+";
    }
    m_str = format("%s%s", m_str.c_str(), sym);
}
void RegNodeVisitor_Printer::visit(const RegNode_Capture* node)
{
    node->node->acceptVisitor(this);
    m_str = format("(%s)", m_str.c_str());
}
