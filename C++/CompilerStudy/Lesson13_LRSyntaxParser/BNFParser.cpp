#include "pch.h"

#include "BNFParser.h"

static bool parseSyntaxSymbol(
        const char *&src, SyntaxSymbol& sym)
{
    while (isspace(*src)) ++src;
    if (src[0] == 0) return false;
    else if (src[0] == '"') {
        const char *end = src;
        while (*++end != '"');
        sym.type = SyntaxSymbol::T_Terminal;
        sym.value = string(src + 1, end);
        src = end + 1;
    }
    else if (isalpha(src[0]) || src[0] == '_') {
        const char *end = src + 1;
        while (isalnum(*end) || *end == '_') ++end;
        sym.type = SyntaxSymbol::T_NonTerminal;
        sym.value = string(src, end);
        src = end;
    }
    else {
        sym.type = SyntaxSymbol::T_NonTerminal;
        sym.value = string(src, src + 1);
        ++src;
    }
    return true;
}

static bool parseProductionList(
        const char *&src, ProductionList& list)
{
    SyntaxSymbol head;
    if (!parseSyntaxSymbol(src, head)) return false;
    assert(head.type == SyntaxSymbol::T_NonTerminal);

    Production production;
    production.push_back(head);
    SyntaxSymbol sym;
    while (parseSyntaxSymbol(src, sym)) {
        if (sym.value == ";") {
            list.push_back(production);
            break;
        }
        else if (sym.value == "|") {
            list.push_back(production);
            production.clear();
            production.push_back(head);
        }
        else if (sym.value == ":") {
        }
        else {
            production.push_back(sym);
        }
    }
    return true;
}

ostream& operator << (ostream& so, const SyntaxSymbol& sym)
{
    if (sym.type == SyntaxSymbol::T_NonTerminal) {
        so << sym.value << ' ';
    }
    else so << '"' << sym.value << '"' << ' ';
    return so;
}

static void dumpProductionList(
        ostream& so, const ProductionList& list)
{
    assert(!list.empty());
    so << list[0][0].value << "\n";
    for (int i = 0; i < (int)list.size(); ++i) {
        if (i == 0) so << "\t: ";
        else so << "\t| ";

        auto &production = list[i];
        for (int i = 1; i < (int)production.size(); ++i) {
            so << production[i];
        }
        so << "\n";
    }
    so << "\t;\n";
}

void BNFInstance::parse(istream& si)
{
    string src;
    for (string line; getline(si, line); src += line + '\n');
    src = "BNFStart:Start\"" END_TERM "\";" + src;

    const char *p = src.c_str();
    while (*p) {
        ProductionList list;
        if (parseProductionList(p, list)) {
            ProductionList &dlist(bnf[list[0][0]]);
            dlist.insert(dlist.end(), list.begin(), list.end());

            for (auto &production : dlist) {
                int ID = (int)ID2production.size();
                ID2production.push_back(&production);
                production2ID[&production] = ID;
            }
        }
    }

    for (auto production : ID2production) {
        for (auto &sym : *production) {
            symbolSet.insert(sym);
        }
    }
}

void BNFInstance::dump(ostream& so)
{
    for (auto &v : bnf) {
        dumpProductionList(so, v.second);
    }
}

ostream& operator << (ostream& so, const Production& p)
{
    so << p[0] << " : ";
    for (int i = 1; i < (int)p.size(); ++i) {
        so << p[i];
    }
    so << ";";
    return so;
}

bool BNFInstance::isNullable(Production::const_iterator begin, Production::const_iterator end) const
{
    if (begin == end) return true;
    for (; begin != end; ++begin) {
        if (!isNullable(*begin)) return false;
    }
    return true;
}
set<string> BNFInstance::getFirst(Production::const_iterator begin, Production::const_iterator end) const
{ 
    set<string> r;
    for (; begin != end; ++begin) {
        const set<string>& subS = getFirst(*begin);
        r.insert(subS.begin(), subS.end());
        if (!isNullable(*begin)) break;
    }
    return r;
}

bool BNFInstance::isNullable(const SyntaxSymbol& sym) const
{
    if (sym.type == SyntaxSymbol::T_Terminal) return false;
    if (m_nullable.count(sym) > 0) return m_nullable[sym];
    bool &b(m_nullable[sym]);
    const ProductionList &list = bnf.find(sym)->second;
    for (auto& production : list) {
        if (isNullable(production.begin() + 1, production.end())) {
            b = true;
            break;
        }
    }
    return b;
}
const set<string>& BNFInstance::getFirst(const SyntaxSymbol& sym) const
{
    if (m_firstSet.count(sym) > 0) return m_firstSet[sym];
    set<string> &r(m_firstSet[sym]);
    if (sym.type == SyntaxSymbol::T_Terminal) {
        r.insert(sym.value);
        return r;
    }
    for (auto &production : bnf.find(sym)->second) {
        set<string> s(getFirst(production.begin() + 1, production.end()));
        r.insert(s.begin(), s.end());
    }
    return r;
}
const set<string>& BNFInstance::getFollow(const SyntaxSymbol& sym) const
{
    if (m_followSet.count(sym) > 0) return m_followSet[sym];
    set<string> &r(m_followSet[sym]);
    for (auto production : ID2production) {
        for (auto iter = production->begin() + 1; iter != production->end(); ++iter) {
            if (*iter == sym) {
                {
                    set<string> subS(getFirst(iter + 1, production->end()));
                    r.insert(subS.begin(), subS.end());
                }
                if (isNullable(iter + 1, production->end())) {
                    const set<string>& subS(getFollow(*production->begin()));
                    r.insert(subS.begin(), subS.end());
                }
            }
        }
    }
    return r;
}
