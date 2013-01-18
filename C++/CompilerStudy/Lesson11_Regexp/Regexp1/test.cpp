#include "pch.h" 

#include <assert.h>
#include <string.h>

#include <set>
#include <vector>
#include <bitset>
#include <memory>
#include <sstream>

#ifdef _MSC_VER
#pragma warning(disable : 4018)
#endif

//==================== regex syntax analysis

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
const char *REG_SPECIAL_CHAR = "\\{}[]().?*+-|";

struct ReNode_Charset;
struct ReNode_Capture;
struct ReNode_Repeat;
struct ReNode_Concat;
struct ReNode_Or;
struct IReNodeVisitor
{
    virtual ~IReNodeVisitor(){}
    virtual void visit(ReNode_Charset *v) = 0;
    virtual void visit(ReNode_Capture *v) = 0;
    virtual void visit(ReNode_Repeat *v) = 0;
    virtual void visit(ReNode_Concat *v) = 0;
    virtual void visit(ReNode_Or *v) = 0;
};

const int REPEAT_MAX = 1000000;
struct IReNode
{
    virtual ~IReNode(){}
    virtual void acceptVisitor(IReNodeVisitor *v) = 0;
};
typedef std::shared_ptr<IReNode> ReNodePtr;
struct ReNode_Charset:
    public IReNode
{
    std::bitset<128> sets;
    virtual void acceptVisitor(IReNodeVisitor *v) { v->visit(this);}
};
struct ReNode_Capture:
    public IReNode
{
    int capID;
    ReNodePtr node;
    ReNode_Capture(const ReNodePtr& n): node(n){}
    virtual void acceptVisitor(IReNodeVisitor *v) { v->visit(this);}
};
struct ReNode_Repeat:
    public IReNode
{
    int min, max;
    bool gready;
    ReNodePtr node;
    ReNode_Repeat(int min, int max, bool gready, const ReNodePtr& n): node(n)
    { this->min = min, this->max = max, this->gready = gready;}
    virtual void acceptVisitor(IReNodeVisitor *v) { v->visit(this);}
};
struct ReNode_Concat:
    public IReNode
{
    ReNodePtr left, right;
    ReNode_Concat(const ReNodePtr& l, const ReNodePtr& r):left(l), right(r){}
    virtual void acceptVisitor(IReNodeVisitor *v) { v->visit(this);}
};
struct ReNode_Or:
    public IReNode
{
    ReNodePtr left, right;
    ReNode_Or(const ReNodePtr& l, const ReNodePtr& r):left(l), right(r){}
    virtual void acceptVisitor(IReNodeVisitor *v) { v->visit(this);}
};

class Parser
{
public:
    Parser(const std::string& src):
        m_src(src), m_curToken(0)
    {
        m_node = sa_expr();
    }
    ReNodePtr& getNode() { return m_node; }
private:
    ReNodePtr sa_expr()
    {
        return sa_or();
    }
    ReNodePtr sa_or()
    {
        if (ReNodePtr left = sa_concat()) {
            return _sa_or(left);
        }
        else return ReNodePtr();
    }
    ReNodePtr _sa_or(const ReNodePtr& left)
    {
        if (!tryConsume('|')) return left;
        ReNodePtr right = sa_concat();
        assert(right);
        return _sa_or(ReNodePtr(new ReNode_Or(left, right)));
    }
    ReNodePtr sa_concat()
    {
        if (ReNodePtr left = sa_repeat()) {
            return _sa_concat(left);
        }
        else return ReNodePtr();
    }
    ReNodePtr _sa_concat(const ReNodePtr& left)
    {
        if (ReNodePtr right = sa_repeat()) {
            return _sa_concat(ReNodePtr(new ReNode_Concat(left, right)));
        }
        else return left;
    }
    ReNodePtr sa_repeat()
    {
        ReNodePtr r = sa_unit();
        if (tryConsume('?')) {
            ReNode_Repeat *repeat = new ReNode_Repeat(0, 1, true, r);
            r.reset(repeat);
        }
        else if (tryConsume('*')) {
            ReNode_Repeat *repeat = new ReNode_Repeat(0, REPEAT_MAX, true, r);
            r.reset(repeat);
        }
        else if (tryConsume('+')) {
            ReNode_Repeat *repeat = new ReNode_Repeat(1, REPEAT_MAX, true, r);
            r.reset(repeat);
        }
        else if (tryConsume('{')) {
            int min, max;
            assert(sa_int(min));
            max = min;
            if (tryConsume(',')) {
                sa_int(max);
            }
            consumeToken('}');
            ReNode_Repeat *repeat = new ReNode_Repeat(min, max, true, r);
            r.reset(repeat);
        }
        else ;
        if (r && tryConsume('?')) { 
            static_cast<ReNode_Repeat*>(r.get())->gready = false;
        }
        return r;
    }
    ReNodePtr sa_unit()
    {
        ReNodePtr r(sa_capture());
        if (!r) {
            r = sa_charset();
        }
        return r;
    }
    ReNodePtr sa_capture()
    {
        if (tryConsume('(')) {
            ReNode_Capture *cap = new ReNode_Capture(sa_expr());
            ReNodePtr r(cap);
            consumeToken(')');
            return r;
        }
        return ReNodePtr();
    }
    ReNodePtr sa_charset()
    {
        ReNode_Charset *p = new ReNode_Charset();
        ReNodePtr r(p);
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
            if (!p->sets.any()) return ReNodePtr();
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
                    char c2;
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
    ReNodePtr m_node;
    std::vector<int> m_backupTokenPos;
};

void printChar(std::ostringstream& so, char c)
{
    if (strchr(REG_SPECIAL_CHAR, c)) so << '\\';
    so << c;
}
void printCharRange(std::ostringstream& so, const std::bitset<128>& sets)
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

class ReNodeVisitor_Printer:
    public IReNodeVisitor
{
public:
    std::string apply(ReNodePtr node)
    {
        m_so.str("");
        node->acceptVisitor(this);
        return m_so.str();
    }

private:
    virtual void visit(ReNode_Charset *v)
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
    virtual void visit(ReNode_Capture *v)
    {
        m_so << '(';
        v->node->acceptVisitor(this);
        m_so << ')';
    }
    virtual void visit(ReNode_Repeat *v)
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
    virtual void visit(ReNode_Concat *v)
    {
        v->left->acceptVisitor(this);
        v->right->acceptVisitor(this);
    }
    virtual void visit(ReNode_Or *v)
    {
        v->left->acceptVisitor(this);
        m_so << "|";
        v->right->acceptVisitor(this);
    }
private:
    std::ostringstream m_so;
};

class ReNodeVisitor_LogicPrinter:
    public IReNodeVisitor
{
public:
    std::string apply(const ReNodePtr& node) 
    {
        m_so.str("");
        node->acceptVisitor(this);
        return m_so.str();
    }
private:
    virtual void visit(ReNode_Charset *v)
    {
        printCharRange(m_so, v->sets);
    }
    virtual void visit(ReNode_Capture *v)
    {
        m_so << '(';
        v->node->acceptVisitor(this);
        m_so << ')';
    }
    virtual void visit(ReNode_Repeat *v)
    {
        v->node->acceptVisitor(this);
        m_so << '{' << v->min << ',' << v->max << '}';
        if (!v->gready) m_so << '?';
    }
    virtual void visit(ReNode_Concat *v)
    {
        v->left->acceptVisitor(this);
        v->right->acceptVisitor(this);
    }
    virtual void visit(ReNode_Or *v)
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

//==================== nfa builder
struct NFANode;
typedef std::shared_ptr<NFANode> NFANodePtr;
struct TransiationEntry
{
    NFANodePtr node;
    std::bitset<128> sets;
};
typedef std::vector<TransiationEntry> TransiationEntryList;
typedef std::vector<NFANodePtr> NFANodeList;
struct NFANode
{
    enum Type
    {
        T_Normal,
        T_Start,
        T_Accept,
    };
    Type type;
    NFANodeList emptyEntries;
    TransiationEntryList entries;
    NFANode(Type t): type(t){}
    void resetType() { type = T_Normal;}
};

class ReNodeVisior_NFABuilder:
    public IReNodeVisitor
{
public:
    NFANodePtr apply(const ReNodePtr &node)
    {
        node->acceptVisitor(this);
        return m_start;
    }
private:
    virtual void visit(ReNode_Charset *v)
    {
        m_start = NFANodePtr(new NFANode(NFANode::T_Start));
        m_accept = NFANodePtr(new NFANode(NFANode::T_Accept));
        TransiationEntry entry = {m_accept};
        entry.sets = v->sets;
        m_start->entries.push_back(entry);
    }
    virtual void visit(ReNode_Capture *v)
    {
        v->node->acceptVisitor(this);
    }
    virtual void visit(ReNode_Repeat *v)
    {
        // only support ?,*,+
        assert(v->min == 0 || v->min == 1);
        assert(v->max == 1 || v->max == REPEAT_MAX);
        v->node->acceptVisitor(this);
        NFANodePtr n1 = m_start, n2 = m_accept;
        m_start = NFANodePtr(new NFANode(NFANode::T_Start));
        m_accept = NFANodePtr(new NFANode(NFANode::T_Accept));
        n1->resetType(); n2->resetType();
        m_start->emptyEntries.push_back(n1);
        n2->emptyEntries.push_back(m_accept);
        if (v->min == 0) {
            m_start->emptyEntries.push_back(m_accept);
        }
        if (v->max == REPEAT_MAX) {
            n2->emptyEntries.push_back(n1);
        }
    }
    virtual void visit(ReNode_Concat *v)
    {
        v->left->acceptVisitor(this);
        NFANodePtr n1 = m_start, n2 = m_accept;
        v->right->acceptVisitor(this);
        NFANodePtr n3 = m_start, n4 = m_accept;
        m_start = n1, m_accept = n4;
        *n2 = *n3;
        n2->resetType();
    }
    virtual void visit(ReNode_Or *v)
    {
        v->left->acceptVisitor(this);
        NFANodePtr n1 = m_start, n2 = m_accept;
        v->right->acceptVisitor(this);
        NFANodePtr n3 = m_start, n4 = m_accept;
        m_start = NFANodePtr(new NFANode(NFANode::T_Start));
        m_accept = NFANodePtr(new NFANode(NFANode::T_Accept));
        n1->resetType(); n2->resetType();
        n3->resetType(); n4->resetType();
        m_start->emptyEntries.push_back(n1);
        m_start->emptyEntries.push_back(n3);
        n2->emptyEntries.push_back(m_accept);
        n4->emptyEntries.push_back(m_accept);
    }
private:
    NFANodePtr m_start, m_accept;
};

typedef std::set<NFANode*> NFANodeSet;
class NFARuntime
{
public:
    NFARuntime(const NFANodePtr& start):
        m_setIdx(0)
    {
        insertNode(m_sets[m_setIdx], start.get());
    }
    bool transit(char c)
    {
        int newIdx = 1 - m_setIdx;
        NFANodeSet &oldSet = m_sets[m_setIdx], &newSet = m_sets[newIdx];
        m_setIdx = newIdx;
        newSet.clear();
        for (NFANodeSet::const_iterator iter = oldSet.begin(); iter != oldSet.end(); ++iter) {
            NFANode *node = *iter;
            for (TransiationEntryList::const_iterator iter = node->entries.begin(); iter != node->entries.end(); ++iter) {
                if (iter->sets.test(c)) {
                    insertNode(newSet, iter->node.get());
                }
            }
        }
        return !newSet.empty();
    }
    bool isAccept() const
    {
        const NFANodeSet &set = m_sets[m_setIdx];
        for (NFANodeSet::const_iterator iter = set.begin(); iter != set.end(); ++iter) {
            if ((*iter)->type == NFANode::T_Accept) return true;
        }
        return false;
    }
private:
    void insertNode(NFANodeSet& sets, NFANode *n)
    {
        if (sets.insert(n).second) {
            for (NFANodeList::const_iterator iter = n->emptyEntries.begin(); iter != n->emptyEntries.end(); ++iter) {
                insertNode(sets, iter->get());
            }
        }
    }
private:
    int m_setIdx;
    NFANodeSet m_sets[2];
};

bool match(const std::string& src, NFANodePtr p)
{
    NFARuntime r(p);
    for (int i = 0; i < src.size(); ++i) if (!r.transit(src[i])) break;
    return r.isAccept();
}

int main()
{
    Parser reg("[\\a_][\\w_]*|[1-9][0-9]*\\.?[0-9]*");
    cout << "reg : " << ReNodeVisitor_Printer().apply(reg.getNode()) << endl;
    cout << "reg : " << ReNodeVisitor_LogicPrinter().apply(reg.getNode()) << endl;
    NFANodePtr nfaNode = ReNodeVisior_NFABuilder().apply(reg.getNode());
    for (std::string line; getline(cin, line);) {
        if (line.empty()) break;
        cout << match(line, nfaNode) << endl;
    }
}
