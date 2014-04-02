
#include "pch.h"
#include "Common.h"
#include "CLRParser.h"

struct CLRItem
{
    int productionID : 12;
    int pos : 8;
    int termID : 12;
    static CLRItem fromInt(int value) 
    {
        CLRItem r;
        *(int*)&r = value;
        return r;
    }
    int toInt() const { return *(int*)this; }
};
struct CLRItemCollection
{
    set<int> items;
    bool operator < (const CLRItemCollection& o) const
    {
        return items < o.items;
    }
    bool operator == (const CLRItemCollection& o) const
    {
        return items == o.items;
    }
};
struct CLRItemCollectionFamily
{
    vector<CLRItemCollection> ID2Collection;
    map<CLRItemCollection, int> collection2ID;
    vector<string> ID2Term;
    map<string, int> term2ID;
    map<int, map<SyntaxSymbol, int> > dfa;
    void build(const BNFInstance& bnf);
private:
    void closure(const BNFInstance& bnf, CLRItemCollection& col);
    void closureItem(const BNFInstance& bnf, CLRItemCollection& col, int item);
    bool addCollection(const CLRItemCollection& col, int &state);
    void transToNewItemCollection(
            const BNFInstance& bnf,
            const CLRItemCollection& col, CLRItemCollection& newCol, const SyntaxSymbol& sym);
};

void CLRItemCollectionFamily::closure(const BNFInstance& bnf, CLRItemCollection& col)
{
    vector<int> items(col.items.begin(), col.items.end());
    col.items.clear();
    for (auto item : items) closureItem(bnf, col, item);
}
void CLRItemCollectionFamily::closureItem(const BNFInstance& bnf, CLRItemCollection& col, int _item)
{
    if (!col.items.insert(_item).second) return;
    CLRItem item(CLRItem::fromInt(_item));
    const Production* production = bnf.ID2production[item.productionID];
    if (item.pos + 1 >= (int)production->size()) return;
    auto &sym((*production)[item.pos + 1]);
    if (sym.type == SyntaxSymbol::T_Terminal) return;
    vector<SyntaxSymbol> seq(production->begin() + item.pos + 2, production->end());
    seq.push_back(SyntaxSymbol(SyntaxSymbol::T_Terminal, ID2Term[item.termID]));
    set<string> first(bnf.getFirst(seq.begin(), seq.end()));
    for (auto &production : bnf.bnf.find(sym)->second) {
        for (auto &term : first) {
            CLRItem newItem = {bnf.production2ID.find((Production*)&production)->second, 0, term2ID[term]};
            closureItem(bnf, col, newItem.toInt());
        }
    }
}
bool CLRItemCollectionFamily::addCollection(const CLRItemCollection& col, int &state)
{
    if (collection2ID.count(col) > 0) {
        state = collection2ID[col];
        return false;
    }
    state = collection2ID[col] = (int)ID2Collection.size();
    ID2Collection.push_back(col);
    return true;
}
void CLRItemCollectionFamily::transToNewItemCollection(
    const BNFInstance& bnf,
    const CLRItemCollection& col, CLRItemCollection& newCol, const SyntaxSymbol& sym)
{
    for (auto _item : col.items) {
        CLRItem item(CLRItem::fromInt(_item));
        const Production* production = bnf.ID2production[item.productionID]; 
        if (item.pos + 1 < (int)production->size() && (*production)[item.pos + 1] == sym) {
            CLRItem newItem = {item.productionID, item.pos + 1, item.termID};
            newCol.items.insert(newItem.toInt());
        }
    }
}

void CLRItemCollectionFamily::build(const BNFInstance& bnf)
{
    for (auto &sym : bnf.symbolSet) {
        if (sym.type == SyntaxSymbol::T_Terminal) {
            term2ID[sym.value] = (int)ID2Term.size();
            ID2Term.push_back(sym.value);
        }
    }

    vector<int> unhanldedState;
    {
        CLRItemCollection col;
        CLRItem item = {0, 0, term2ID[END_TERM]};
        col.items.insert(item.toInt());
        closure(bnf, col);
        int ns;
        addCollection(col, ns);
        unhanldedState.push_back(0);
    }
    while (!unhanldedState.empty()) {
        int state = unhanldedState.back();
        unhanldedState.pop_back();
        for (auto &sym : bnf.symbolSet) {
            const CLRItemCollection& col(ID2Collection[state]);
            CLRItemCollection newCol;
            transToNewItemCollection(bnf, col, newCol, sym);
            closure(bnf, newCol);
            if (!newCol.items.empty()) {
                int ns;
                if (addCollection(newCol, ns)) {
                    unhanldedState.push_back(ns);
                }
                dfa[state][sym] = ns;
            }
        }
    }
}
// ----------
Action CLRActionTable::getAction(int state, const string& term)
{
    ASSERT(actionMap.count(state) > 0, format("%d,%s", state, term.c_str()));
    ASSERT(actionMap[state].count(term) > 0, format("%d,%s", state, term.c_str()));
    return actionMap[state][term];
}
int CLRActionTable::getStateCount()
{
    return (int)actionMap.size();
}

int CLRGotoTable::getNextState(int state, const string& nonTerm)
{
    ASSERT(gotoMap.count(state) > 0, format("%d,%s", state, nonTerm.c_str()));
    ASSERT(gotoMap[state].count(nonTerm) > 0, format("%d,%s", state, nonTerm.c_str()));
    return gotoMap[state][nonTerm];
}

CLRParserTable::CLRParserTable():
    m_actionTable(NULL), m_gotoTable(NULL), m_bnf(NULL)
{
}
CLRParserTable::~CLRParserTable()
{
    if (m_actionTable != NULL) delete m_actionTable;
    if (m_gotoTable != NULL) delete m_gotoTable;
}
IActionTable* CLRParserTable::getActionTable()
{
    return m_actionTable;
}
IGotoTable* CLRParserTable::getGotoTable()
{
    return m_gotoTable;
}
const BNFInstance* CLRParserTable::getBNF()
{
    return m_bnf;
}

void CLRParserTable::init(const BNFInstance& bnf)
{
    m_bnf = &bnf;

    CLRItemCollectionFamily family;
    family.build(bnf);

    m_gotoTable = new CLRGotoTable();
    for (auto &p : family.dfa) {
        for (auto &p2 : p.second) {
            if (p2.first.type == SyntaxSymbol::T_NonTerminal) {
                m_gotoTable->gotoMap[p.first][p2.first.value] = p2.second;
            }
        }
    }

    m_actionTable = new CLRActionTable();
    for (int state = 0; state < (int)family.ID2Collection.size(); ++state) {
        map<string, int> shiftMap;
        map<string, int> reduceMap;

        for (auto &p2 : family.dfa[state]) {
            if (p2.first.type == SyntaxSymbol::T_Terminal) {
                shiftMap[p2.first.value] = p2.second;
            }
        }
        for (auto &_item : family.ID2Collection[state].items) {
            CLRItem item(CLRItem::fromInt(_item));
            const Production* production = bnf.ID2production[item.productionID];
            if (production->size() - 1 == item.pos) {
                string term = family.ID2Term[item.termID];
                ASSERT(shiftMap.count(term) == 0, "shift & reduce conflict");
                ASSERT(reduceMap.count(term) == 0, "reduce & reduce conflict");
                reduceMap[term] = item.productionID;
            }
        }

        auto &m = m_actionTable->actionMap[state];
        for (auto &p : shiftMap) {
            m[p.first] = Action(Action::T_Shift, p.second);
        }
        for (auto &p : reduceMap) {
            m[p.first] = Action(Action::T_Reduce, p.second);
        }
    }
}
int CLRParserTable::getStartState()
{
    return 0;
}
