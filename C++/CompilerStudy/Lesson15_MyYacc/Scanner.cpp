
#include "pch.h"

#include "CodeGen.h"
#include "RegParser.h"
#include "Scanner.h"
#include "DynamicBitset.h"

const char *yytext;
int yyleng;
YYSTYPE yylval;
static int yyline_no;
int yyget_lineno()
{
    return yyline_no;
}
void yyset_lineno(int l)
{
    yyline_no = l;
}

struct RegDFA
{
    vector<int> stateTrans;
    vector<int> state2Reg;
    vector<int(*)()> reg2Action;
    void newState()
    {
        stateTrans.resize(stateTrans.size() + 256, -1);
        state2Reg.resize(state2Reg.size() + 1, -1);
    }
    void setTrans(int state, char c, int nextState)
    {
        stateTrans[state * 256 + (unsigned char)c] = nextState;
    }
    int getTrans(int state, char c)
    {
        return stateTrans[state * 256 + (unsigned char)c];
    }
};

class RegNodeVisitor_DFABuilder:
    public IRegNodeVisitor
{
private:
    enum VisitorTrun
    {
        VT_SetID,
        VT_SetNullable,
        VT_SetFirst,
        VT_SetLast,
        VT_SetFollow,
    };
public:
    RegNodeVisitor_DFABuilder(RegDFA *dfa)
    {
        buildNodesInfo();
        buildDFA(dfa);
    }
private:
    void buildNodesInfo()
    {
        for (auto &reg : g_regs) {
            m_nodes.push_back(RegParser(reg).getRoot());
        }
        for (int i = 0; i < (int)m_nodes.size(); ++i) {
            auto &node = m_nodes[i];
            auto p = new RegNode_Charset();
            m_node2Reg[p] = i;
            node.reset(new RegNode_Concat(node, RegNodePtr(p)));
        }
        while (m_nodes.size() > 1) {
            auto r = m_nodes[m_nodes.size() - 1];
            auto l = m_nodes[m_nodes.size() - 2];
            m_nodes.erase(m_nodes.end() - 1);
            m_nodes.back().reset(new RegNode_Or(l, r));
        }

        m_trun = VT_SetID; m_nodes[0]->acceptVisitor(this);
        m_follow.resize(m_ID2Node.size(), DynamicBitset((int)m_ID2Node.size()));

        m_trun = VT_SetNullable; m_nodes[0]->acceptVisitor(this);
        m_trun = VT_SetFirst; m_nodes[0]->acceptVisitor(this);
        m_trun = VT_SetLast; m_nodes[0]->acceptVisitor(this);
        m_trun = VT_SetFollow; m_nodes[0]->acceptVisitor(this);
    }
    void buildDFA(RegDFA *dfa)
    {
        for (auto &reg : g_regs) {
            dfa->reg2Action.push_back(g_reg2Action[reg]);
        }

        map<DynamicBitset, int> set2State;
        vector<DynamicBitset> state2Set;
        state2Set.push_back(m_firsts[m_nodes[0].get()]);
        set2State[state2Set.back()] = 0;
        dfa->newState();
        vector<int> unhanled;
        unhanled.push_back(0);
        while (!unhanled.empty()) {
            int state = unhanled.back();
            unhanled.pop_back();
            vector<int> ints(state2Set[state].toInts());
            for (int c = 0; c < 256; ++c) {
                DynamicBitset newSet((int)m_ID2Node.size());
                for (auto i : ints) {
                    if (m_ID2Node[i]->chars[c]) {
                        newSet.unionWith(m_follow[i]);
                    }
                }
                if (newSet.any()) {
                    int nState = 0;
                    if (set2State.count(newSet)) {
                        nState = set2State[newSet];
                    }
                    else {
                        nState = set2State[newSet] = (int)state2Set.size();
                        state2Set.push_back(newSet);
                        dfa->newState();
                        unhanled.push_back(nState);
                    }
                    dfa->setTrans(state, c, nState);
                }
            }
        }

        for (auto &p : set2State) {
            int reg = -1;
            for (auto i : p.first.toInts()) {
                auto node = m_ID2Node[i];
                if (m_node2Reg.count(node)) {
                    int _reg = m_node2Reg[node];
                    if (reg == -1 || _reg < reg) {
                        reg = _reg;
                    }
                }
            }
            if (reg != -1) {
                dfa->state2Reg[p.second] = reg;
            }
        }
    }
private:
    virtual void visit(const RegNode_Charset* node)
    {
        switch (m_trun) {
            case VT_SetID:
                m_node2ID[node] = (int)m_ID2Node.size();
                m_ID2Node.push_back(node);
                break;
            case VT_SetNullable:
                m_nullable[node] = false;
                break;
            case VT_SetFirst:
                {
                    DynamicBitset set((int)m_ID2Node.size());
                    set.set(m_node2ID[node]);
                    m_firsts[node] = set;
                }
                break;
            case VT_SetLast:
                {
                    DynamicBitset set((int)m_ID2Node.size());
                    set.set(m_node2ID[node]);
                    m_lasts[node] = set;
                }
                break;
            case VT_SetFollow:
                break;
            default: ASSERT(0);
        }
    }
    virtual void visit(const RegNode_Concat* node)
    {
        node->left->acceptVisitor(this);
        node->right->acceptVisitor(this);
        switch (m_trun) {
            case VT_SetID:
                break;
            case VT_SetNullable:
                m_nullable[node] = m_nullable[node->left.get()] && m_nullable[node->right.get()]; 
                break;
            case VT_SetFirst:
                m_firsts[node] = m_firsts[node->left.get()];
                if (m_nullable[node->left.get()]) {
                    m_firsts[node].unionWith(m_firsts[node->right.get()]);
                }
                break;
            case VT_SetLast:
                m_lasts[node] = m_lasts[node->right.get()];
                if (m_nullable[node->right.get()]) {
                    m_lasts[node].unionWith(m_lasts[node->left.get()]);
                }
                break;
            case VT_SetFollow:
                {
                    DynamicBitset set(m_firsts[node->right.get()]);
                    for (auto &id : m_lasts[node->left.get()].toInts()) {
                        m_follow[id].unionWith(set);
                    }
                }
                break;
            default: ASSERT(0);
        }
    }
    virtual void visit(const RegNode_Or* node)
    {
        node->left->acceptVisitor(this);
        node->right->acceptVisitor(this);
        switch (m_trun) {
            case VT_SetID:
                break;
            case VT_SetNullable:
                m_nullable[node] = m_nullable[node->left.get()] || m_nullable[node->right.get()];
                break;
            case VT_SetFirst:
                m_firsts[node] = m_firsts[node->left.get()];
                m_firsts[node].unionWith(m_firsts[node->right.get()]);
                break;
            case VT_SetLast:
                m_lasts[node] = m_lasts[node->left.get()];
                m_lasts[node].unionWith(m_lasts[node->right.get()]);
                break;
            case VT_SetFollow:
                break;
            default: ASSERT(0);
        }
    }
    virtual void visit(const RegNode_Repeat* node)
    {
        node->node->acceptVisitor(this);
        switch (m_trun) {
            case VT_SetID:
                break;
            case VT_SetNullable:
                m_nullable[node] = node->min == 0;
                break;
            case VT_SetFirst:
                m_firsts[node] = m_firsts[node->node.get()];
                break;
            case VT_SetLast:
                m_lasts[node] = m_lasts[node->node.get()];
                break;
            case VT_SetFollow:
                if (node->max > 1) {
                    DynamicBitset set(m_firsts[node->node.get()]);
                    for (auto &id : m_lasts[node->node.get()].toInts()) {
                        m_follow[id].unionWith(set);
                    }
                }
                break;
            default: ASSERT(0);
        }
    }
    virtual void visit(const RegNode_Capture* node)
    {
        node->node->acceptVisitor(this);
        switch (m_trun) {
            case VT_SetID:
                break;
            case VT_SetNullable:
                m_nullable[node] = m_nullable[node->node.get()];
                break;
            case VT_SetFirst:
                m_firsts[node] = m_firsts[node->node.get()];
                break;
            case VT_SetLast:
                m_lasts[node] = m_lasts[node->node.get()];
                break;
            case VT_SetFollow:
                break;
            default: ASSERT(0);
        }
    }
private:
    vector<RegNodePtr> m_nodes;
    map<const RegNode_Charset*, int> m_node2ID;
    vector<const RegNode_Charset*> m_ID2Node;
    map<const RegNode_Charset*, int> m_node2Reg;
    map<const IRegNode*, bool> m_nullable;
    map<const IRegNode*, DynamicBitset> m_firsts;
    map<const IRegNode*, DynamicBitset> m_lasts;
    vector<DynamicBitset> m_follow;
    int m_trun;
};

class ScannerImpl
{
public:
    ScannerImpl(const string& src);
    ~ScannerImpl();
    bool getNext(Token& token);
private:
    string m_src;
    int m_curPos;
    RegDFA* m_dfa;
};
shared_ptr<RegDFA> g_dfa;
ScannerImpl::ScannerImpl(const string& src):
    m_src(src), m_curPos(0)
{
    if (g_dfa == 0) {
        g_dfa.reset(new RegDFA());
        (RegNodeVisitor_DFABuilder(g_dfa.get()));
    }
    m_dfa = g_dfa.get();
}
ScannerImpl::~ScannerImpl()
{
}
bool ScannerImpl::getNext(Token& token)
{
    for (;;) {
        if (m_curPos >= (int)m_src.size()) return false;

        int pos = m_curPos;
        int state = 0, newState;
        int reg = -1, rpos = 0;
        while (pos < m_src.size() &&
                (newState = m_dfa->getTrans(state, m_src[pos])) != -1) {
            state = newState;
            ++pos;
            int _reg = m_dfa->state2Reg[state];
            if (_reg != -1) {
                reg = _reg;
                rpos = pos;
            }
        }
        ASSERT(reg != -1);

        yytext = m_src.c_str() + m_curPos;
        yyleng = rpos - m_curPos;
        m_curPos = rpos;

        char *yytextEnd = (char*)yytext + yyleng;
        char bc = yytextEnd[0];
        yytextEnd[0] = 0;

        token.type = m_dfa->reg2Action[reg]();

        yytextEnd[0] = bc;
        if (token.type > 0) return true;
    }
    return false;
}

// Scanner
Scanner::Scanner(const string& src):
    m_impl(new ScannerImpl(src))
{
}
Scanner::~Scanner()
{
    delete m_impl;
}
bool Scanner::getNext(Token& token)
{
    return m_impl->getNext(token);
}
