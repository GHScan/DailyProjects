
#ifndef BE_SYMBOL_TABLE_H
#define BE_SYMBOL_TABLE_H

struct BEType;
class BESymbolTable;

struct BESymbol {
    BESymbolTable *parent;
    string name;
    const BEType* type;
    int off;
};

class BESymbolTable {
public:
    BESymbolTable(BESymbolTable *prevTable);
    ~BESymbolTable();

    BESymbolTable* getPrevTable();
    BESymbol* declare(const string &name, const BEType *type);
    void undeclare(const string& name);
    BESymbol* get(const string &name);
    int getStartOff() const;
    int getEndOff() const;
    int getMaxEndOff() const;
    vector<BESymbol*> getSymbols();
private:
    BESymbolTable *m_prevTable;
    int m_startOff, m_endOff;
    int m_maxEndOff;
    map<string, BESymbol> m_symbols;
};

#endif
