
#include "pch.h"
#include "Common.h"
#include "SLRParser.h"

// SLRItemCollectionFamily
struct SLRItem
{
    int productionID : 20;
    int pos : 12;
    static SLRItem fromInt(int i) 
    {
        SLRItem item;
        *(int*)&item = i;
        return item;
    }
    int toInt() { return *(int*)this;}
};
struct SLRItemCollection
{
    set<int> items;
    bool operator == (const SLRItemCollection& col) const
    {
        return items == col.items;
    }
    bool operator < (const SLRItemCollection& col) const
    {
        return items < col.items;
    }
};
struct SLRItemCollectionFamily
{
    vector<SLRItemCollection> ID2ItemCollection;
    map<SLRItemCollection, int> itemCollection2ID;
    map<int, map<SyntaxSymbol, int> > dfa;
    void build(const BNFInstance& bnf);
private:
    void closure(const BNFInstance& bnf, SLRItemCollection& col);
    void closureItem(const BNFInstance& bnf, SLRItemCollection& col, int item);
    bool addCollection(const SLRItemCollection& col, int &state);
    void transToNewItemCollection(
            const BNFInstance& bnf,
            const SLRItemCollection& col, SLRItemCollection& newCol, const SyntaxSymbol& sym);
};
void SLRItemCollectionFamily::transToNewItemCollection(
        const BNFInstance& bnf,
        const SLRItemCollection& col, SLRItemCollection& newCol, const SyntaxSymbol& sym)
{
    for (auto _item : col.items) {
        SLRItem item(SLRItem::fromInt(_item));
        const Production* production = bnf.ID2production[item.productionID];
        if (item.pos + 1 < (int)production->size() && (*production)[item.pos + 1] == sym) {
            SLRItem newItem = {item.productionID, item.pos + 1};
            newCol.items.insert(newItem.toInt());
        }
    }
}
void SLRItemCollectionFamily::build(const BNFInstance& bnf)
{
    vector<int> unhanldedState;
    {
        SLRItemCollection col;
        SLRItem item = {0, 0};
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
            const SLRItemCollection& col(ID2ItemCollection[state]);
            SLRItemCollection newCol;
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
void SLRItemCollectionFamily::closureItem(const BNFInstance& bnf, SLRItemCollection& col, int _item)
{
    if (!col.items.insert(_item).second) return;
    SLRItem item(SLRItem::fromInt(_item));
    const Production* production = bnf.ID2production[item.productionID];
    if (item.pos + 1 >= (int)production->size()) return;
    const SyntaxSymbol& sym(*(production->begin() + 1 + item.pos));
    if (sym.type == SyntaxSymbol::T_Terminal) return;
    for (auto &production : bnf.bnf.find(sym)->second) {
        SLRItem newItem = {bnf.production2ID.find((Production*)&production)->second, 0};
        closureItem(bnf, col, newItem.toInt());
    }
}
void SLRItemCollectionFamily::closure(const BNFInstance& bnf, SLRItemCollection& col)
{
    vector<int> items(col.items.begin(), col.items.end());
    col.items.clear();
    for (auto item : items) closureItem(bnf, col, item);
}
bool SLRItemCollectionFamily::addCollection(const SLRItemCollection& col, int &state)
{
    assert(!col.items.empty());
    if (itemCollection2ID.count(col) > 0) {
        state = itemCollection2ID[col];
        return false;
    }
    state = (int)ID2ItemCollection.size();
    itemCollection2ID[col] = state;
    ID2ItemCollection.push_back(col);
    return true;
}

// SLRActionTable
Action SLRActionTable::getAction(int state, const string& term)
{
    ASSERT(actionMap.count(state) > 0, format("%d,%s", state, term.c_str()));
    ASSERT(actionMap[state].count(term) > 0, format("%d,%s", state, term.c_str()));
    return actionMap[state][term];
}
int SLRActionTable::getStateCount()
{
    return (int)actionMap.size();
}

// SLRGotoTable
int SLRGotoTable::getNextState(int state, const string& nonTerm)
{
    ASSERT(gotoMap.count(state) > 0, format("%d,%s", state, nonTerm.c_str()));
    ASSERT(gotoMap[state].count(nonTerm) > 0, format("%d,%s", state, nonTerm.c_str()));
    return gotoMap[state][nonTerm];
}

// SLRParserTable
SLRParserTable::SLRParserTable():
    m_bnf(NULL), m_actionTable(NULL), m_gotoTable(NULL)
{
}
SLRParserTable::~SLRParserTable()
{
    if (m_actionTable != NULL) delete m_actionTable;
    if (m_gotoTable != NULL) delete m_gotoTable;
}

IActionTable* SLRParserTable::getActionTable()
{
    return m_actionTable;
}
IGotoTable* SLRParserTable::getGotoTable()
{
    return m_gotoTable;
}
const BNFInstance* SLRParserTable::getBNF()
{
    return m_bnf;
}

void SLRParserTable::init(const BNFInstance& bnf)
{
    m_bnf = &bnf;

    SLRItemCollectionFamily family;
    family.build(bnf);

    m_gotoTable = new SLRGotoTable();
    for (auto &p : family.dfa) {
        for (auto &p2 : p.second) {
            if (p2.first.type == SyntaxSymbol::T_NonTerminal) {
                m_gotoTable->gotoMap[p.first][p2.first.value] = p2.second;
            }
        }
    }

    m_actionTable = new SLRActionTable();
    for (int state = 0; state < (int)family.ID2ItemCollection.size(); ++state) {
        map<string, int> shiftMap;
        map<string, int> reduceMap;

        for (auto &p2 : family.dfa[state]) {
            if (p2.first.type == SyntaxSymbol::T_Terminal) {
                shiftMap[p2.first.value] = p2.second;
            }
        }
        for (auto &_item : family.ID2ItemCollection[state].items) {
            SLRItem item(SLRItem::fromInt(_item));
            const Production* production = bnf.ID2production[item.productionID];
            if (production->size() - 1 == item.pos) {
                const set<string>& fo = bnf.getFollow(*production->begin());
                for (auto &s : fo) {
                    ASSERT(shiftMap.count(s) == 0, "shift & reduce conflict");
                    ASSERT(reduceMap.count(s) == 0, "reduce & reduce conflict");
                    reduceMap[s] = item.productionID;
                }
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
int SLRParserTable::getStartState()
{
    return 0;
}
