
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
    int getBlockSize() const;
    int getBlockOff() const;

    void declareLocal(const string& name);
    int getLocalIndex(const string& name) const;
    int getUpValueIndex(const string& name);
    int getLocalCount() const;

private:
    static vector<SymbolTable> s_stack;

private:
    int m_lastLocalIdx, m_maxLocalIdx;
    vector<map<string, int> > m_blocks;
    map<string, int> m_upValues;
};
#endif
