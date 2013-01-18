
#ifndef BNFPARSER_H
#define BNFPARSER_H

#define END_TERM "__$"

struct SyntaxSymbol
{
    enum Type
    {
        T_Terminal = 1,
        T_NonTerminal,
    };
    Type type;
    string value;

    SyntaxSymbol(): type(T_NonTerminal){}
    SyntaxSymbol(Type t, const string& value) { this->type = t, this->value = value; }
    bool operator == (const SyntaxSymbol& o) const
    {
        return type == o.type && value == o.value;
    }
    bool operator < (const SyntaxSymbol& o) const
    {
        if (type == o.type) {
            return value < o.value;
        }
        return type < o.type;
    }
};
ostream& operator << (ostream& so, const SyntaxSymbol& sym);

typedef vector<SyntaxSymbol> Production;
ostream& operator << (ostream& so, const Production& p);

typedef vector<Production> ProductionList;

class BNFInstance
{
public:
    map<SyntaxSymbol, ProductionList> bnf;
    vector<Production*> ID2production;
    map<Production*, int> production2ID;
    set<SyntaxSymbol> symbolSet;
    void parse(istream& si);
    void dump(ostream& so);
    const set<string>& getFirst(const SyntaxSymbol& sym) const;
    set<string> getFirst(Production::const_iterator begin, Production::const_iterator end) const;
    const set<string>& getFollow(const SyntaxSymbol& sym) const;
private:
    bool isNullable(const SyntaxSymbol& sym) const;
    bool isNullable(Production::const_iterator begin, Production::const_iterator end) const;
private:
    mutable map<SyntaxSymbol, set<string> > m_firstSet;
    mutable map<SyntaxSymbol, set<string> > m_followSet;
    mutable map<SyntaxSymbol, bool> m_nullable;
};

#endif
