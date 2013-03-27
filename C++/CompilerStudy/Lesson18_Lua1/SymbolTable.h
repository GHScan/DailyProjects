
#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

class SymbolTable {
public:
    static void push();
    static void pop();
    static SymbolTable* top();

    SymbolTable();
    ~SymbolTable();

    void beginBlock();
    void endBlock();

    void declareLocal(const string& name);
    int getLocalIndex(const string& name);
    int getUpValueIndex(const string& name);

private:
    static vector<SymbolTable> s_stack;

private:
    int m_lastLocalIdx;
    vector<map<string, int> > m_blocks;
    map<string, int> m_upValues;
};
#endif
