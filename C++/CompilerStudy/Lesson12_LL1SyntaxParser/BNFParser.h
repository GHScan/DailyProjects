
#ifndef GRAMMAR_PARSER_H
#define GRAMMAR_PARSER_H

#include <string>
#include <vector>
#include <map>
#include <memory>

struct SyntaxSymbol
{
    enum Type
    {
        T_Terminal,
        T_NoneTerminal,
    };
    Type type;
    std::string value;

    bool operator < (const SyntaxSymbol& o) const
    {
        if (type == o.type) return value < o.value;
        return type < o.type;
    }
    bool operator == (const SyntaxSymbol& o) const
    {
        return type == o.type && value == o.value;
    }
};
typedef std::vector<SyntaxSymbol> SyntaxSymbolList;

struct BNFProduction
{
    std::string head;
    SyntaxSymbolList body;
};
typedef std::vector<BNFProduction> BNFProductionList;
typedef std::map<std::string, BNFProductionList> BNFStruct;

class PredictiveAnalysisTable
{
public:
    PredictiveAnalysisTable(const BNFStruct &bnf);
    ~PredictiveAnalysisTable();
    int next(const std::string& head, const std::string& term);
    void dump(std::ostream& so);
private:
    class PredictiveAnalysisTable_Impl *m_impl;
};

std::istream& operator >> (std::istream& si, BNFStruct &bnf);
std::ostream& operator << (std::ostream& so, const BNFStruct &bnf);
void optimizeBNF(BNFStruct &bnf);

#endif
