
#include "pch.h"

#include <stdarg.h>

#include <set>

#include "BNFParser.h"

#define EMPTY_STRING "_E"

static std::string format(const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    static char buf[512] = "";
    vsprintf(buf, fmt, arg);
    va_end(arg);
    return buf;
}

class BNFProductionParser
{
public:
    BNFProductionParser(const std::string& src)
    {
        SyntaxSymbolList list;
        parseSymbolList(src, list);

        BNFProduction product;
        assert(list[0].type == SyntaxSymbol::T_NoneTerminal);
        product.head = list[0].value;

        assert(list[1].type == SyntaxSymbol::T_NoneTerminal && list[1].value == "-");
        assert(list[2].type == SyntaxSymbol::T_NoneTerminal && list[2].value == ">");

        for (int i = 3; i < (int)list.size(); ++i) {
            if (list[i].type == SyntaxSymbol::T_NoneTerminal && list[i].value == "|") {
                m_list.push_back(product);
                product.body.clear();
            }
            else if (list[i].type == SyntaxSymbol::T_NoneTerminal && list[i].value == EMPTY_STRING) {
                assert(product.body.empty());
                m_list.push_back(product);
            }
            else {
                product.body.push_back(list[i]);
            }
        }
        if (!product.body.empty()) {
            m_list.push_back(product);
        }
    }
    const BNFProductionList& getList() const { return m_list; }
private:
    void parseSymbolList(const std::string& _src, SyntaxSymbolList &list)
    {
        const char *src = _src.c_str();
        for (;;) {
            while (isspace(src[0])) ++src;
            if (src[0] == 0) break;
            if (src[0] == '"') {
                const char *end = src;
                while (*++end != '"');
                SyntaxSymbol sym = {SyntaxSymbol::T_Terminal, std::string(src + 1, end)};
                list.push_back(sym);
                src = end + 1;
            }
            else if (isalpha(src[0]) || src[0] == '_') {
                const char *end = src + 1;
                while (isdigit(end[0]) || isalpha(end[0]) || end[0] == '_') ++end;
                SyntaxSymbol sym = {SyntaxSymbol::T_NoneTerminal, std::string(src, end)};
                list.push_back(sym);
                src = end;
            }
            else {
                SyntaxSymbol sym = {SyntaxSymbol::T_NoneTerminal, std::string(src, src + 1)};
                list.push_back(sym);
                ++src;
            }
        }
    }
private:
    BNFProductionList m_list;
};

std::istream& operator >> (std::istream& si, BNFStruct &bnf)
{
    for (std::string line; getline(si, line); ) {
        if (line.empty()) continue;
        BNFProductionParser p(line);
        BNFProductionList &list = bnf[p.getList()[0].head];
        list.insert(list.end(), p.getList().begin(), p.getList().end());
    }
    return si;
}

std::ostream& operator << (std::ostream& so, const SyntaxSymbol& sym)
{
    if (sym.type == SyntaxSymbol::T_NoneTerminal) {
        so << sym.value;
    }
    else if (sym.type == SyntaxSymbol::T_Terminal) {
        so << '"' << sym.value << '"';
    }
    return so;
}
std::ostream& operator << (std::ostream& so, const BNFStruct &bnf)
{
    for (BNFStruct::const_iterator iter = bnf.begin(); 
            iter != bnf.end();
            ++iter) {
        const BNFProductionList& list = iter->second;
        int n = 0;
        for (BNFProductionList::const_iterator iter = list.begin(); iter != list.end(); ++iter) {
            if (n == 0) so << iter->head << " -> ";
            const BNFProduction& product = *iter;
            if (product.body.empty()) {
                so << EMPTY_STRING;
            }
            else {
                for (SyntaxSymbolList::const_iterator iter = product.body.begin(); iter != product.body.end(); ++iter) {
                    so << *iter << ' ';
                }
            }
            n += (int)iter->body.size() + 1;
            if (n > 30 || (iter + 1) == list.end()) {
                n = 0;
                so << "\n";
            }
            else so << " | ";
        }
    }
    return so;
}

static std::string genDerivedSymbolName(const std::string& name)
{
    assert(!name.empty());
    if (!isdigit(name.back())) return name + "1";
    const char *last = name.c_str() + name.size() - 1;
    while (last >= name.c_str() && isdigit(*last)) --last;
    int n = atoi(last + 1);
    char buf[32] = "";
    sprintf(buf, "%d", n + 1);
    return name.substr(0, last - name.c_str() + 1) + buf;
}

static void unpackProduction(BNFStruct &bnf, const std::string& head, const std::string& unpackHead)
{
    BNFProductionList &list(bnf[head]);
    const BNFProductionList &unpackList(bnf[unpackHead]);
    for (;;) {
        BNFProductionList::iterator iter = list.begin();
        for (; iter != list.end(); ++iter) {
            if (iter->body.empty()) continue;
            if (iter->body[0].type == SyntaxSymbol::T_NoneTerminal && iter->body[0].value == unpackHead) {
                break;
            }
        }
        if (iter == list.end()) break;

        BNFProduction product(*iter);
        list.erase(iter);
        product.body.erase(product.body.begin());

        for (BNFProductionList::const_iterator iter = unpackList.begin(); iter != unpackList.end(); ++iter) {
            BNFProduction newProduct(product);
            newProduct.body.insert(newProduct.body.begin(), iter->body.begin(), iter->body.end());
            list.push_back(newProduct);
        }
    }
}
static void removeDirectLeftRecursive(BNFStruct &bnf, const std::string& head, BNFStruct &newBnf)
{
    std::string newHead = genDerivedSymbolName(head);
    SyntaxSymbol sym = {SyntaxSymbol::T_NoneTerminal, newHead};
    BNFProductionList &list(bnf[head]);
    BNFProductionList leftList;
    for (BNFProductionList::iterator iter = list.begin(); iter != list.end(); ) {
        if (!iter->body.empty() && iter->body[0].type == SyntaxSymbol::T_NoneTerminal && iter->body[0].value == head) {
            iter->body.erase(iter->body.begin());
            assert(!iter->body.empty());
            iter->body.push_back(sym);
            iter->head = newHead;
            leftList.push_back(*iter);
            iter = list.erase(iter);
        }
        else ++iter;
    }
    if (leftList.empty()) return;

    {
        BNFProduction product;
        product.head = newHead;
        leftList.push_back(product);
    }

    newBnf[newHead] = leftList;
    for (BNFProductionList::iterator iter = list.begin(); iter != list.end(); ++iter) {
        iter->body.push_back(sym);
    }
}

static void removeLeftRecursive(BNFStruct &bnf)
{
    BNFStruct newBnf;
    for (BNFStruct::iterator iter = bnf.begin(); iter != bnf.end(); ++iter) {
        for (BNFStruct::iterator iter2 = bnf.begin(); iter2 != iter; ++iter2) {
            unpackProduction(bnf, iter->first, iter2->first);
        }
        removeDirectLeftRecursive(bnf, iter->first, newBnf);
    }
    bnf.insert(newBnf.begin(), newBnf.end());
}

static void removeLeftCommonFactor(BNFStruct &bnf)
{
    for (;;) {
        bool modified = false;

        for (BNFStruct::iterator iter = bnf.begin(); iter != bnf.end(); ++iter) {
            BNFProductionList &list = iter->second;
            std::vector<int> commons;
            int n = 0;
            {
                std::map<std::string, int> m;
                for (int i = 0; i < (int)list.size(); ++i) {
                    if (list[i].body.empty()) continue;
                    std::string s = list[i].body[0].value;
                    if (m.count(s) == 0) m[s] = i;
                    else {
                        if (commons.empty()) {
                            commons.push_back(m[s]);
                            commons.push_back(i);
                        }
                        else {
                            if (s == list[commons[0]].body[0].value) {
                                commons.push_back(i);
                            }
                        }
                    }
                }
                if (!commons.empty()) {
                    n = 1;
                    bool end = false;
                    for (; list[commons[0]].body.size() > n; ++n) {
                        for (int i = 1; i < (int)commons.size(); ++i) {
                            if (list[commons[i]].body.size() <= n || 
                                    list[commons[i]].body[n].value != list[commons[0]].body[n].value) {
                                end = true;
                                break;
                            }
                        }
                        if (end) break;
                    }
                }
            }
            if (!commons.empty()) {
                std::string newName = genDerivedSymbolName(iter->first);
                BNFProductionList &newList = bnf[newName];
                {
                    BNFProduction newProduct;
                    newProduct.head = newName;
                    for (int i = 0; i < (int)commons.size(); ++i) {
                        newProduct.body.clear();
                        const BNFProduction& product = iter->second[commons[i]];
                        newProduct.body.assign(product.body.begin() + n, product.body.end());
                        newList.push_back(newProduct);
                    }
                }

                {
                    BNFProduction newProduct;
                    newProduct.head = iter->first;
                    newProduct.body.assign(list[commons[0]].body.begin(), list[commons[0]].body.begin() + n);
                    {
                        SyntaxSymbol sym = {SyntaxSymbol::T_NoneTerminal, newName};
                        newProduct.body.push_back(sym);
                    }
                    list.push_back(newProduct);
                }
                for (int i = (int)commons.size() - 1; i >= 0; --i) {
                    list.erase(list.begin() + commons[i]);
                }

                modified = true;
                break;
            }
        }

        if (!modified) break;
    }
}

void optimizeBNF(BNFStruct &bnf)
{
    removeLeftRecursive(bnf);
    removeLeftCommonFactor(bnf);
}

static bool isSymbolEmptiable(const SyntaxSymbol& sym, const BNFStruct &bnf)
{
    if (sym.type == SyntaxSymbol::T_Terminal) return false;
    const BNFProductionList &list(bnf.find(sym.value)->second);
    for (int i = 0; i < (int)list.size(); ++i) {
        if (list[i].body.empty()) return true;
    }
    return false;
}

typedef std::set<std::string> TermSet;
typedef std::map<SyntaxSymbol, TermSet> Symbol2TermSet;

class PredictiveAnalysisTable_Impl
{
public:
    PredictiveAnalysisTable_Impl(const BNFStruct &bnf);
    int next(const std::string& head, const std::string& term);
    void dump(std::ostream& so);
private:
    typedef std::map<std::string, std::map<std::string, int> > TableT;
private:
    void buildAnalysisTable(const BNFStruct& bnf);
private:
    TableT m_table;
};

static const TermSet& getSymbolFirst(const SyntaxSymbol& sym, const BNFStruct &bnf, Symbol2TermSet &firstSet)
{
    if (firstSet.count(sym) > 0) return firstSet[sym];
    TermSet &set(firstSet[sym]);
    if (sym.type == SyntaxSymbol::T_Terminal) {
        set.insert(sym.value);
    }
    else {
        const BNFProductionList &list(bnf.find(sym.value)->second);
        for (BNFProductionList::const_iterator iter = list.begin(); iter != list.end(); ++iter) {
            const SyntaxSymbolList &symList(iter->body);
            for (int i = 0; i < (int)symList.size(); ++i) {
                const TermSet& sub(getSymbolFirst(symList[i], bnf, firstSet));
                set.insert(sub.begin(), sub.end());
                if (!isSymbolEmptiable(symList[i], bnf)) break;
            }
        }
    }
    return set;
}
static const TermSet& getSymbolFollow(const SyntaxSymbol& sym, const BNFStruct &bnf, Symbol2TermSet &followSet, Symbol2TermSet &firstSet)
{
    if (followSet.count(sym) > 0) return followSet[sym];
    assert(sym.type == SyntaxSymbol::T_NoneTerminal);
    TermSet &set(followSet[sym]);
    for (BNFStruct::const_iterator iter = bnf.begin(); iter != bnf.end(); ++iter) {
        const BNFProductionList &list = iter->second;
        for (BNFProductionList::const_iterator iter = list.begin(); iter != list.end(); ++iter) {
            for (int i = 0; i < (int)iter->body.size(); ++i) {
                if (sym == iter->body[i] && i + 1 < iter->body.size()) {
                    const TermSet& sub(getSymbolFirst(iter->body[i + 1], bnf, firstSet));
                    set.insert(sub.begin(), sub.end());
                }
            }
        }
    }
    for (BNFStruct::const_iterator iter = bnf.begin(); iter != bnf.end(); ++iter) {
        const BNFProductionList &list = iter->second;
        for (BNFProductionList::const_iterator iter = list.begin(); iter != list.end(); ++iter) {
            for (int i = 0; i < (int)iter->body.size(); ++i) {
                if (sym == iter->body[i]) {
                    bool b = true;
                    for (int j = i + 1; j < (int)iter->body.size(); ++j) {
                        if (!isSymbolEmptiable(iter->body[j], bnf)) {
                            b = false;
                            break;
                        }
                    }
                    if (b) {
                        SyntaxSymbol headSym = {SyntaxSymbol::T_NoneTerminal, iter->head};
                        const TermSet &sub(getSymbolFollow(headSym, bnf, followSet, firstSet));
                        set.insert(sub.begin(), sub.end());
                    }
                }
            }
        }
    }
    return set;
}

static void collectSymbolTerms(const BNFStruct& bnf, TermSet& set)
{
    for (BNFStruct::const_iterator iter = bnf.begin(); iter != bnf.end(); ++iter) {
        for (BNFProductionList::const_iterator iter2 = iter->second.begin(); iter2 != iter->second.end(); ++iter2) {
            for (SyntaxSymbolList::const_iterator iter3 = iter2->body.begin(); iter3 != iter2->body.end(); ++iter3) {
                if (iter3->type == SyntaxSymbol::T_Terminal) set.insert(iter3->value);
            }
        }
    }
}

PredictiveAnalysisTable_Impl::PredictiveAnalysisTable_Impl(const BNFStruct &bnf)
{
    buildAnalysisTable(bnf);
}

void PredictiveAnalysisTable_Impl::buildAnalysisTable(const BNFStruct& bnf)
{
    TermSet termSet;
    collectSymbolTerms(bnf, termSet);

    Symbol2TermSet firstSet, followSet;
    for (BNFStruct::const_iterator iter = bnf.begin(); iter != bnf.end(); ++iter) {
        const BNFProductionList &list(iter->second);
        TableT::mapped_type &m(m_table[iter->first]);
        for (TermSet::const_iterator iter = termSet.begin(); iter != termSet.end(); ++iter) {
            m[*iter] = -1;
        }
        for (BNFProductionList::const_iterator iter = list.begin(); iter != list.end(); ++iter) {
            int i = (int)(iter - list.begin());
            TermSet set;
            if (iter->body.empty()) {
                SyntaxSymbol sym = {SyntaxSymbol::T_NoneTerminal, iter->head};
                set = getSymbolFollow(sym, bnf, followSet, firstSet);
            }
            else {
                set = getSymbolFirst(iter->body[0], bnf, firstSet);
            }
            for (TermSet::const_iterator iter = set.begin(); iter != set.end(); ++iter) {
                assert(m[*iter] == -1 && "The grammar must be LL(1) !");
                m[*iter] = i;
            }
        }
    }
}

int PredictiveAnalysisTable_Impl::next(const std::string& head, const std::string& term)
{
    assert(m_table.count(head) > 0 && m_table[head].count(term) > 0);
    return m_table[head][term];
}
void PredictiveAnalysisTable_Impl::dump(std::ostream& so)
{
    std::vector<std::string> terms;
    for (std::map<std::string, int>::iterator iter = m_table.begin()->second.begin(); 
            iter != m_table.begin()->second.end(); ++iter) {
        terms.push_back(iter->first);
    }

    so << format("%-6s", "");
    for (int i = 0; i < (int)terms.size(); ++i) {
        so << '\t' << format("%-6s", terms[i].c_str());
    }
    so << '\n';

    for (TableT::iterator iter = m_table.begin(); iter != m_table.end(); ++iter) {
        so << format("%-6s", iter->first.c_str());
        for (int i = 0; i < (int)terms.size(); ++i) {
            so << '\t' << format("%-6d", iter->second[terms[i]]);
        }
        so << '\n';
    }
}

PredictiveAnalysisTable::PredictiveAnalysisTable(const BNFStruct &bnf):
    m_impl(new PredictiveAnalysisTable_Impl(bnf))
{ }
PredictiveAnalysisTable::~PredictiveAnalysisTable()
{
    delete m_impl;
}
int PredictiveAnalysisTable::next(const std::string& head, const std::string& term)
{
     return m_impl->next(head, term);
}
void PredictiveAnalysisTable::dump(std::ostream& so)
{
    m_impl->dump(so);
}
