
#include "pch.h"
#include "Common.h"
#include "LALRParser.h"

namespace 
{
    struct LR0Item
    {
        int productionID : 24;
        int pos : 8;
        bool operator == (const LR0Item& o) const { return (int&)*this == (int&)o;}
        bool operator < (const LR0Item& o) const { return (int&)*this < (int&)o; }
    };
    struct LR0ItemCollection
    {
        set<LR0Item> items;
        bool operator < (const LR0ItemCollection& o)  const { return items < o.items; }
        bool operator == (const LR0ItemCollection& o) const { return items == o.items; }
    };
    struct LR0ItemCollectionFamily
    {
        vector<LR0ItemCollection> ID2Collection;
        map<LR0ItemCollection, int> collection2ID;
        map<int, map<SyntaxSymbol, int> > translateTable;
        void build(const BNFInstance& bnf);
        void translate(const BNFInstance& bnf, const LR0ItemCollection& col, const SyntaxSymbol& sym, LR0ItemCollection& newCol);
        void closure(const BNFInstance& bnf, LR0ItemCollection& col);
        void closureItem(const BNFInstance& bnf, LR0ItemCollection& col, LR0Item item);
        bool addCollection(const LR0ItemCollection& col, int &id);
        void removeNonCore();
    };
    void LR0ItemCollectionFamily::build(const BNFInstance& bnf)
    {
        set<int> unhandled;
        {
            LR0Item item = {0, 0};
            LR0ItemCollection col;
            col.items.insert(item);
            closure(bnf, col);
            int id;
            addCollection(col, id);
            unhandled.insert(id);
        }
        while (!unhandled.empty()) {
            int id = *unhandled.begin();
            unhandled.erase(unhandled.begin());
            for (auto &sym : bnf.symbolSet) {
                const LR0ItemCollection& col = ID2Collection[id];
                LR0ItemCollection newCol;
                translate(bnf, col, sym, newCol);
                if (!newCol.items.empty()) {
                    closure(bnf, newCol);
                    int nid;
                    if (addCollection(newCol, nid)) {
                        unhandled.insert(nid);
                    }
                    translateTable[id][sym] = nid;
                }
            }
        }
        removeNonCore();
    }
    void LR0ItemCollectionFamily::translate(const BNFInstance& bnf, const LR0ItemCollection& col, const SyntaxSymbol& sym, LR0ItemCollection& newCol)
    {
        for (auto item : col.items) {
            const Production* production = bnf.ID2production[item.productionID];
            if (item.pos + 1 < (int)production->size() && (*production)[item.pos + 1] == sym) {
                LR0Item newItem = {item.productionID, item.pos + 1};
                newCol.items.insert(newItem);
            }
        }
    }
    void LR0ItemCollectionFamily::closureItem(const BNFInstance& bnf, LR0ItemCollection& col, LR0Item item)
    {
        if (col.items.count(item) > 0) return;
        col.items.insert(item);
        const Production* production = bnf.ID2production[item.productionID];
        if (item.pos + 1 >= (int)production->size()) return;
        const SyntaxSymbol &sym = (*production)[item.pos + 1];
        if (sym.type == SyntaxSymbol::T_Terminal) return;
        for (auto &production : bnf.bnf.find(sym)->second) {
            LR0Item newItem = {bnf.production2ID.find((Production*)&production)->second, 0};
            closureItem(bnf, col, newItem);
        }
    }
    void LR0ItemCollectionFamily::closure(const BNFInstance& bnf, LR0ItemCollection& col)
    {
        vector<LR0Item> items(col.items.begin(), col.items.end());
        col.items.clear();
        for (auto item : items) closureItem(bnf, col, item);
    }
    bool LR0ItemCollectionFamily::addCollection(const LR0ItemCollection& col, int &id)
    {
        if (collection2ID.count(col) > 0) {
            id = collection2ID[col];
            return false;
        }
        id = collection2ID[col] = (int)ID2Collection.size();
        ID2Collection.push_back(col);
        return true;
    }
    void LR0ItemCollectionFamily::removeNonCore()
    {
        collection2ID.clear();
        for (int i = 0; i < (int)ID2Collection.size(); ++i) {
            LR0ItemCollection newCol;
            for (auto item : ID2Collection[i].items) {
                if (item.pos > 0 || item.productionID == 0) newCol.items.insert(item);
            }
            ID2Collection[i] = newCol;
            collection2ID[newCol] = i;
        }
    }

    struct LR1Item
    {
        int productionID : 12;
        int pos : 6;
        int termID : 14;
        bool operator == (const LR1Item& o) const { return (int&)*this == (int&)o; }
        bool operator < (const LR1Item& o) const { return (int&)*this < (int&)o; }
    };
}

struct LALRPoint
{
    int state;
    LR0Item item;
    bool operator == (const LALRPoint& o) const { return (long long&)*this == (long long&)o;}
    bool operator < (const LALRPoint& o) const { return (long long&)*this < (long long&)o;}
};

struct LALRCollectionFamily
{
    map<SyntaxSymbol, int> symbol2ID;
    vector<SyntaxSymbol> ID2Symbol;
    void build(const BNFInstance &bnf, LALRStateActionTable& table);
    void closureItem(const BNFInstance& bnf, set<LR1Item> &col, LR1Item item);
};

void LALRCollectionFamily::closureItem(const BNFInstance& bnf, set<LR1Item> &col, LR1Item item)
{
    if (col.count(item) > 0) return ;
    col.insert(item);
    const Production *production = bnf.ID2production[item.productionID];
    if (item.pos + 1 >= (int)production->size()) return;
    const SyntaxSymbol &sym = (*production)[item.pos + 1];
    if (sym.type == SyntaxSymbol::T_Terminal) return;
    set<string> first;
    {
        vector<SyntaxSymbol> seq(production->begin() + item.pos + 2, production->end());
        seq.push_back(ID2Symbol[item.termID]);
        first = bnf.getFirst(seq.begin(), seq.end());
    }
    for (auto &production : bnf.bnf.find(sym)->second) {
        LR1Item newItem = {bnf.production2ID.find((Production*)&production)->second, 0, 0};
        for (auto term : first) {
            newItem.termID = symbol2ID[SyntaxSymbol(SyntaxSymbol::T_Terminal, term)];
            closureItem(bnf, col, newItem);
        }
    }
}
void LALRCollectionFamily::build(const BNFInstance &bnf, LALRStateActionTable& table)
{
    LR0ItemCollectionFamily LR0Family;
    LR0Family.build(bnf);

    {
        int i = 0;
        for (auto &sym : bnf.symbolSet) {
            symbol2ID[sym] = i++;
            ID2Symbol.push_back(sym);
        }
        ID2Symbol.push_back(SyntaxSymbol(SyntaxSymbol::T_Terminal, "3k5pz19FEj42H"));
        symbol2ID[ID2Symbol.back()] = i++;
    }

    map<LALRPoint, set<LALRPoint> > point2point;
    map<LALRPoint, set<int> > point2TermSet;
    set<LALRPoint> unhandled;

    for (int i = 0; i < (int)LR0Family.ID2Collection.size(); ++i) {
        for (auto item : LR0Family.ID2Collection[i].items) {
            LALRPoint pt = { i, item };
            set<LR1Item> col;
            {
                LR1Item _item = {item.productionID, item.pos, (int)ID2Symbol.size() - 1};
                closureItem(bnf, col, _item);
            }
            for (auto r1Item : col) {
                LALRPoint _pt = {i, {r1Item.productionID, r1Item.pos}};
                if (r1Item.termID == (int)ID2Symbol.size() - 1) {
                    point2point[pt].insert(_pt);
                }
                else {
                    point2TermSet[_pt].insert(r1Item.termID);
                    unhandled.insert(_pt);
                }
                const Production *production = bnf.ID2production[r1Item.productionID];
                if (r1Item.pos + 1 >= (int)production->size()) continue;
                int ns = LR0Family.translateTable[i][(*production)[r1Item.pos + 1]];
                LR0Item nitem = {r1Item.productionID, r1Item.pos + 1};
                assert(LR0Family.ID2Collection[ns].items.count(nitem) > 0);
                LALRPoint npt = {ns, nitem};
                point2point[_pt].insert(npt);
            }
        }
    }

    while (!unhandled.empty()) {
        LALRPoint pt = *unhandled.begin();
        unhandled.erase(unhandled.begin());
        for (auto &npt : point2point[pt]) {
            const set<int>& st(point2TermSet[pt]);
            set<int>& nst(point2TermSet[npt]);
            auto oldSize = nst.size();
            nst.insert(st.begin(), st.end());
            if (oldSize != nst.size()) {
                unhandled.insert(npt);
            }
        }
    }

    table.resize(LR0Family.ID2Collection.size());
    for (auto &v : table) v.resize(ID2Symbol.size() - 1, Action(Action::T_Reduce, 0));
    // shift
    for (auto &p : LR0Family.translateTable) {
        for (auto &p2 : p.second) {
            table[p.first][symbol2ID[p2.first]] = Action(Action::T_Shift, p2.second);
        }
    }
    // reduce
    for (auto &p : point2TermSet) {
        const LALRPoint &pt(p.first);
        const Production *production = bnf.ID2production[pt.item.productionID];
        if (pt.item.pos + 1 != production->size()) continue;
        for (auto termID : p.second) {
            ASSERT(table[pt.state][termID].type != Action::T_Shift, "shift & reduce conflict");
            table[pt.state][termID] = Action(Action::T_Reduce, pt.item.productionID);
        }
    }
}

//////////
Action LALRActionTable::getAction(int state, const string& term)
{
    ASSERT(term2ID.count(term) > 0, format("invalid term: %d, %s\n", state, term.c_str()));
    return (*table)[state][term2ID[term]];
}
int LALRActionTable::getStateCount()
{
    return (int)table->size();
}

int LALRGotoTable::getNextState(int state, const string& nonTerm)
{
    ASSERT(nonTerm2ID.count(nonTerm) > 0, format("invalid nonTerm:%d, %s", state, nonTerm.c_str()));
    const Action& act = (*table)[state][nonTerm2ID[nonTerm]];
    ASSERT(act.type == Action::T_Shift, format("must be shift: %d, %s", state, nonTerm.c_str()));
    return act.value;
}

LALRParserTable::LALRParserTable():
    m_bnf(NULL), m_gotoTable(NULL), m_actionTable(NULL)
{
}
LALRParserTable::~LALRParserTable()
{
    if (m_actionTable != NULL) delete m_actionTable;
    if (m_gotoTable != NULL) delete m_gotoTable;
}
IActionTable* LALRParserTable::getActionTable()
{
    return m_actionTable;
}
IGotoTable* LALRParserTable::getGotoTable()
{
    return m_gotoTable;
}
const BNFInstance* LALRParserTable::getBNF()
{
    return m_bnf;
}

void LALRParserTable::init(const BNFInstance& bnf)
{
    m_bnf = &bnf;

    LALRCollectionFamily family;
    family.build(bnf, m_table);

    m_actionTable = new LALRActionTable();
    m_actionTable->table = &m_table;
    m_gotoTable = new LALRGotoTable();
    m_gotoTable->table = &m_table;
    for (auto &p : family.symbol2ID) {
        if (p.first.type == SyntaxSymbol::T_Terminal) {
            m_actionTable->term2ID[p.first.value] = p.second;
        }
        else if (p.first.type == SyntaxSymbol::T_NonTerminal) {
            m_gotoTable->nonTerm2ID[p.first.value] = p.second;
        }
        else assert(0);
    }
}
int LALRParserTable::getStartState()
{
    return 0;
}
