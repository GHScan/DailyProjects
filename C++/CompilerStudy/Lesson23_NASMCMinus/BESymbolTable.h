
#ifndef BE_SYMBOL_TABLE_H
#define BE_SYMBOL_TABLE_H

struct BEType;
class BESymbolTable;

struct BESymbol {
    BESymbolTable *parent;
    string name;
    BEType* type;
    int off;
};

class BESymbolTable {
public:
    BESymbolTable(BESymbolTable *prevTable);
    ~BESymbolTable();

    BESymbol* declare(const string &name, BEType *type);
    void undeclare(const string& name);
    BESymbol* get(const string &name);
    int getStartOff() const;
    int getEndOff() const;

private:
    BESymbolTable *m_prevTable;
    int m_startOff, m_endOff;
    map<string, BESymbol> m_symbols;
};

#endif
