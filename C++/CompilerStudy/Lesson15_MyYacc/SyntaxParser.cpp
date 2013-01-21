
#include "pch.h"

#include "SyntaxParser.h"
#include "CodeGen.h"

YYSTYPE *g_lrproductionHead, *g_lrproductionBody;
extern YYSTYPE& LRProductionHead()
{
    return g_lrproductionHead[0];
}
extern YYSTYPE* LRProductionBody()
{
    return g_lrproductionBody;
}

////////// LALRParser
struct Action
{
    enum Type
    {
        T_Null,
        T_Shift,
        T_Reduce,
    };
    Action(): type(T_Null), value(0){}
    Type type : 2;
    int value : 30;
};
class ActionTable
{
public:
    Action getAction(int state, int term);
    void newState();
    void setAction(int state, int term, Action act);
private:
    vector<map<int, Action> > m_charTable;
    vector<Action> m_termTable;
};
Action ActionTable::getAction(int state, int term)
{
    if (term < ESS_Term_Begin) {
        return m_charTable[state][term];
    }
    else {
        return m_termTable[state * ESS_Term_Ceil + (term - ESS_Term_Begin)];
    }
}
void ActionTable::newState()
{
    m_charTable.resize(m_charTable.size() + 1);
    m_termTable.resize(m_termTable.size() + ESS_Term_Ceil);
}
void ActionTable::setAction(int state, int term, Action act)
{
    if (term < ESS_Term_Begin) {
        m_charTable[state][term] = act;
    }
    else {
        m_termTable[state * ESS_Term_Ceil + (term - ESS_Term_Begin)] = act;
    }
}

class GotoTable
{
public:
    int getNextState(int state, ESyntaxSymbol sym);
    void newState();
    void setNextState(int state, ESyntaxSymbol sym, int nstate);
private:
    vector<int> m_nonTermTable;
};
int GotoTable::getNextState(int state, ESyntaxSymbol sym)
{
    return m_nonTermTable[state * ESS_NonTerm_Ceil + sym];
}
void GotoTable::newState()
{
    m_nonTermTable.resize(m_nonTermTable.size() + ESS_NonTerm_Ceil);
}
void GotoTable::setNextState(int state, ESyntaxSymbol sym, int nstate)
{
    m_nonTermTable[state * ESS_NonTerm_Ceil + sym] = nstate;
}

class LALRParser
{
public:
    GotoTable* getGotoTable() { return &m_gotoTable;}
    ActionTable* getActionTable() { return &m_actionTable;}
    static LALRParser* instance();
    void build();
private:
    LALRParser() { }
private:
    GotoTable m_gotoTable;
    ActionTable m_actionTable;
};
LALRParser* LALRParser::instance()
{
    static LALRParser s_ins;
    return &s_ins;
}

struct LR0Item
{
    int productID : 24;
    int pos : 8;
    LR0Item(): productID(0), pos(0){}
    LR0Item(int pid, int p): productID(pid), pos(p){}
    bool operator < (const LR0Item &o) const { return (int&)*this < (int&)o;}
    bool operator == (const LR0Item &o) const { return (int&)*this == (int&)o;}
};
typedef set<LR0Item> LR0ItemCollection;
struct LR0ItemCollectionFamily
{
    set<LR0ItemCollection> cols;
    void build();
    void closure(LR0ItemCollection &col);
    void removeNoneCore();
};
void LR0ItemCollectionFamily::build()
{
    set<LR0ItemCollection> unhandled;
    {
        LR0ItemCollection c;
        c.insert(LR0Item(0, 0));
        closure(c);
        unhandled.insert(c);
        clos.insert(c);
    }

    while (!unhandled.empty()) {
        LR0ItemCollection col = *unhandled.begin();
        unhandled.erase(unhandled.begin());
        for (auto item : col) {
            auto productBody = getProductBody(item.productID);
            if (productBody.size())
        }
    }

    removeNoneCore();
}
void LR0ItemCollectionFamily::closure(LR0ItemCollection &col)
{
}
void LR0ItemCollectionFamily::removeNoneCore()
{
}

void LALRParser::build()
{
}

////////// ParserImpl
class SyntaxParserImpl
{
public:
    SyntaxParserImpl(Scanner *scanner);
    ~SyntaxParserImpl();
    bool parse();
private:
    Scanner *m_scanner;
};
SyntaxParserImpl::SyntaxParserImpl(Scanner *scanner):
    m_scanner(scanner)
{
    LALRParser::instance()->build();
}
SyntaxParserImpl::~SyntaxParserImpl()
{
}
bool SyntaxParserImpl::parse()
{
    vector<YYSTYPE> valueStack;
    vector<int> stateStack;
    stateStack.push_back(0);

    GotoTable *gotoTable = LALRParser::instance()->getGotoTable();
    ActionTable* actionTable = LALRParser::instance()->getActionTable();

    bool useLastToken = false;
    Token t;
    for (;;) {
        if (!m_scanner->getNext(t)) { 
            if (useLastToken) break;
            else {
                useLastToken = true;
                t.type = ESS_Term_End;
            }
        }

        for (;;) {
            Action act = actionTable->getAction(stateStack.back(), t.type);
            if (act.type == Action::T_Shift) {
                if (t.type == ESS_Term_End) { 
                    stateStack.pop_back();
                    break;
                }
                stateStack.push_back(act.value);
                valueStack.push_back(yylval);
                break;
            }
            else if (act.type == Action::T_Reduce) {
                const vector<int>& productBody = getProductBody(act.value);
                YYSTYPE head;
                g_lrproductionHead = &head;
                g_lrproductionBody = &valueStack[valueStack.size() - productBody.size()];
                getProductionAction(act.value)();
                valueStack.erase(valueStack.end() - productBody.size(), valueStack.end());
                valueStack.push_back(head);
                stateStack.erase(stateStack.end() - productBody.size());
                int nstate = gotoTable->getNextState(stateStack.back(), getProductHead(act.value));
                stateStack.push_back(nstate);
            }
            else ASSERT(0);
        }
    }
    ASSERT(stateStack.empty());

    return true;
}
//////////
SyntaxParser::SyntaxParser(Scanner *scanner):
    m_impl(new SyntaxParserImpl(scanner))
{
}
SyntaxParser::~SyntaxParser()
{
    delete m_impl;
}
bool SyntaxParser::parse()
{
    return m_impl->parse();
}
