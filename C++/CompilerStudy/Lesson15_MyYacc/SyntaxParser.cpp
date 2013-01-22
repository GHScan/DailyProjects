
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
static string productBodyToString(int pid)
{
    string r = toString(getProductHead(pid)) + " : ";
    for (auto sym : getProductBody(pid)) {
        r += toString(sym);
        r += " ";
    }
    r += ";";
    return r;
}

static vector<ESyntaxSymbol> initTermList()
{
    set<ESyntaxSymbol> r;
    for (int pid = 0; pid < PRODUCT_ID_END; ++pid) {
        auto& body = getProductBody(pid);
        for (auto sym : body) {
            if (!isNonTerm(sym)) r.insert(sym);
        }
    }
    return vector<ESyntaxSymbol>(r.begin(), r.end());
}
static vector<ESyntaxSymbol> initNonTermList()
{
    vector<ESyntaxSymbol> r;
    for (auto i = ESS_NonTerm_Begin; i < ESS_NonTerm_End; ++i) {
        r.push_back(i);
    }
    return r;
}
static vector<ESyntaxSymbol> initSymbolList()
{
    auto r = initTermList();
    auto r2 = initNonTermList();
    r.insert(r.end(), r2.begin(), r2.end());
    return r;
}
static auto g_termList = initTermList();
static auto g_nonTermList = initNonTermList();
static auto g_symolList = initSymbolList();
//////////

struct Action
{
    enum Type
    {
        T_Null,
        T_Shift,
        T_Reduce,
    };
    Action(): type(T_Null), value(0){}
    Action(Type t, int val): type(t), value(val){}
    Type type : 2;
    int value : 30;
};
class ActionTable
{
public:
    Action getAction(int state, ESyntaxSymbol term);
    void setStateCount(int cnt);
    void setAction(int state, ESyntaxSymbol term, Action act);
private:
    vector<map<int, Action> > m_charTable;
    vector<Action> m_termTable;
};
Action ActionTable::getAction(int state, ESyntaxSymbol term)
{
    if (term < ESS_Term_Begin) {
        return m_charTable[state][term];
    }
    else {
        return m_termTable[state * ESS_Term_Ceil + (term - ESS_Term_Begin)];
    }
}
void ActionTable::setStateCount(int cnt)
{
    m_charTable.resize(cnt);
    m_termTable.resize(cnt * ESS_Term_Ceil);
}
void ActionTable::setAction(int state, ESyntaxSymbol term, Action act)
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
    void setStateCount(int cnt);
    void setNextState(int state, ESyntaxSymbol sym, int nstate);
private:
    vector<int> m_nonTermTable;
};
int GotoTable::getNextState(int state, ESyntaxSymbol sym)
{
    return m_nonTermTable[state * ESS_NonTerm_Ceil + sym];
}
void GotoTable::setStateCount(int cnt)
{
    m_nonTermTable.resize(cnt * ESS_NonTerm_Ceil, -1);
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
//////////
struct LR0Item
{
    int productID : 24;
    int pos : 8;
    LR0Item(): productID(0), pos(0){}
    LR0Item(int pid, int p): productID(pid), pos(p){}
    bool operator < (const LR0Item &o) const { return (int&)*this < (int&)o;}
    bool operator == (const LR0Item &o) const { return (int&)*this == (int&)o;}
};
static_assert(PRODUCT_ID_END < (1 << 24), "See LR0Item.productID");
static_assert(PRODUCT_BODY_MAX_LEN < (1 << 8), "See LR0Item.pos");

inline bool isImportantLR0Item(int productID, int pos)
{
    if (pos == 0) {
        if (productID == 0 || getProductBody(productID).size() == 0) return true;
        return false;
    }
    return true;
}

typedef set<LR0Item> LR0ItemCollection;
struct LR0ItemCollectionFamily
{
    vector<LR0ItemCollection> ID2Collection;
    map<LR0ItemCollection, int> collection2ID;
    map<int, map<ESyntaxSymbol, int> > transMap;
    void build();
private:
    void closure(LR0ItemCollection &col);
    void closureItem(LR0Item item, LR0ItemCollection& col);
    bool insertCollection(LR0ItemCollection& col, int &ID);
    void removeCore();
};
void LR0ItemCollectionFamily::build()
{
    set<int> unhandled;
    {
        LR0ItemCollection col;
        col.insert(LR0Item(0, 0));
        int ID;
        insertCollection(col, ID);
        unhandled.insert(ID);
    }
    while (!unhandled.empty()) {
        int ID = *unhandled.begin();
        unhandled.erase(unhandled.begin());
        auto& col = ID2Collection[ID];
        for (auto sym : g_symolList) {
            LR0ItemCollection newCol;
            for (auto item : col) {
                auto &body = getProductBody(item.productID);
                if (item.pos < body.size() && body[item.pos] == sym) {
                    newCol.insert(LR0Item(item.productID, item.pos + 1));
                }
            }
            int newID;
            if (insertCollection(newCol, newID)) {
                unhandled.insert(newID);
            }
            transMap[ID][sym] = newID;
        }
    }
    removeCore();
}
void LR0ItemCollectionFamily::closureItem(LR0Item item, LR0ItemCollection& col)
{
    if (!col.insert(item).second) return;
    auto& body = getProductBody(item.productID);
    if (item.pos >= body.size()) return;
    if (!isNonTerm(body[item.pos])) return;
    int pbegin, pend;
    getNonTermProductRange(body[item.pos], begin, end);
    for (int pid = pbegin; pid < pend; ++pid) {
        closureItem(LR0Item(pid, 0), col);
    }
}
void LR0ItemCollectionFamily::closure(LR0ItemCollection &col)
{
    vector<LR0Item> items(col.begin(), col.end());
    col.clear();
    for (auto item : items) closureItem(item, col);
}
bool LR0ItemCollectionFamily::insertCollection(LR0ItemCollection& col, int &ID)
{
    closure(col);
    if (collection2ID.count(col) > 0) {
        ID = collection2ID[col];
        return false;
    }
    ID = collection2ID[col] = (int)ID2Collection.size();
    ID2Collection.push_back(col);
    return true;
}
void LR0ItemCollectionFamily::removeCore()
{
    collection2ID.clear();
    for (int i = 0; i < (int)ID2Collection.size(); ++i) {
        LR0ItemCollection col;
        for (auto item : ID2Collection[i]) {
            if (isImportantLR0Item(item)) col.insert(item);
        }
        ID2Collection[i] = col;
        collection2ID[col] = i;
    }
}
//////////
struct LR1Item
{
    int state : 15;
    int productID : 10;
    int pos : 7;
    LR1Item(int _state, int _productID, int _pos): state(_state), productID(_productID), pos(_pos){}
    bool operator == (const LR1Item& o) { return (int&)*this == (int&)o;}
    bool operator < (const LR1Item& o) { return (int&)*this < (int&)o;}
};
typedef set<LR1Item> LR1ItemCollection;
static_assert(PRODUCT_ID_END < (1 << 10), "See LR1Item.productID");
static_assert(PRODUCT_BODY_MAX_LEN < (1 << 7), "See LR1Item.pos");

//////////
static void mergeAction(Action &dact, Action sact, const LR0ItemCollectionFamily& family)
{
    if (sact.type == Action::T_Null) ASSERT(0);
    else if(sact.type == Action::T_Shift) ASSERT(0);
    else if (sact.type == Action::T_Reduce) {
        if (dact.type == Action::T_Null) dact = sact;
        else if (dact.type == Action::T_Shift) {
            int shiftPid = -1;
            for (auto item : family.ID2Collection[dact.value]) {
                if (item.pos > 0) {
                    if (shiftPid == -1) shiftPid = item.productID;
                    else ASSERT(0); // ???
                }
            }
            int sterm = getProductConflictToken(sact.value);
            int dterm = getProductConflictToken(shiftPid);
            if (sterm == -1 || dterm == -1) {
                ASSERT1(0, "shift & reduce conflict -" + productBodyToString(sact.value) + " & " + productBodyToString(shiftPid));
            }
            if (sterm == dterm) {
                if (getTermAssoc(sterm) == 'l') dact = sact;
            }
            else {
                if (getTermPriority(sterm) > getTermPriority(dterm)) dact = sact;
            }

        }
        else if (dact.type == Action::T_Reduce) {
            int sterm = getProductConflictToken(sact.value);
            int dterm = getProductConflictToken(dact.value);
            if (sterm == -1 || dterm == -1) {
                ASSERT1(0, "reduce & reduce conflict -" + productBodyToString(sact.value) + " & " + productBodyToString(dact.value));
            }
            if (sterm == dterm) {
                ASSERT(0);
            }
            else {
                if (getTermPriority(sterm) > getTermPriority(dterm)) dact = sact;
            }
        }
        else ASSERT(0);
    }
    else ASSERT(0);
}

void LALRParser::build()
{
    LR0ItemCollectionFamily LR0Family;
    LR0Family.build();

    map<LR1Item, set<ESyntaxSymbol> > item2Terms;
    map<LR1Item, set<LR1Item> > item2items;
    set<LR1Item> unhandled;

    // TODO - calc item2Terms, item2items
    item2Terms

    while (!unhandled.empty()) {
        LR1Item item = *unhandled.begin();
        unhandled.erase(unhandled.begin());
        auto& terms = item2Terms[item];
        for (auto ditem : item2items[item]) {
            auto& dterms = item2Terms[ditem].size();
            auto osize = dterms.size();
            dterms.insert(terms.begin(), terms.end());
            if (dterms.size() != osize) {
                unhandled.insert(ditem);
            }
        }
    }

    m_gotoTable.setStateCount(LR0Family.ID2Collection.size());
    m_actionTable.setStateCount(LR0Family.ID2Collection.size());

    for (int state = 0; state < (int)LR0Family.size(); ++state) {
        for (auto term : g_termList) {
            Action act;
            {
                auto &m = LR0Family.transMap[state];
                if (m.count(term)) {
                    mergeAction(act, Action(Action::T_Shift, m[term]), LR0Family);
                }
            }
            for (auto item : LR0Family.ID2Collection[state]) {
                auto& body = getProductBody(item.productID);
                if (item.pos != body.size()) continue;
                auto& terms = item2Terms[LR1Item(state, item.productID, item.pos)];
                if (terms.count(term) == 0) continue;
                mergeAction(act, Action(Action::T_Reduce, item.productID), LR0Family);
            }
            m_actionTable.setAction(state, term, act);
        }
        for (auto nonTerm : g_nonTermList) {
            auto &m = LR0Family.transMap[state];
            if (m.count(nonTerm)) {
                m_gotoTable.setNextState(state, nonTerm, m[nonTerm]);
            }
        }
    }
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
                t.type = ESS_Term_Begin;
            }
        }

        for (;;) {
            Action act = actionTable->getAction(stateStack.back(), t.type);
            if (act.type == Action::T_Shift) {
                if (t.type == ESS_Term_Begin) { 
                    stateStack.pop_back();
                    break;
                }
                stateStack.push_back(act.value);
                valueStack.push_back(yylval);
                break;
            }
            else if (act.type == Action::T_Reduce) {
                auto& body = getProductBody(act.value);
                YYSTYPE head;
                g_lrproductionHead = &head;
                g_lrproductionBody = &valueStack[valueStack.size() - body.size()];
                getProductionAction(act.value)();
                valueStack.erase(valueStack.end() - body.size(), valueStack.end());
                valueStack.push_back(head);
                stateStack.erase(stateStack.end() - body.size());
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
