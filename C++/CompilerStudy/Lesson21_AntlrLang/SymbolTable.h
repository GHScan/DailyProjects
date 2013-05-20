
#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

struct FuncMeta;
typedef shared_ptr<FuncMeta> FuncMetaPtr;

class SymbolTable {
public:
    static SymbolTable* topTable();
    static void pushTable(FuncMetaPtr &meta);
    static void popTable();

    SymbolTable(FuncMetaPtr &meta);
    ~SymbolTable();

    FuncMetaPtr& getMeta();

    void pushBlock();
    void popBlock();
    pair<int, int> getLocalOffSize();
    void declareLocal(const string& name);
    int getLocalIdx(const string& name);
    int getMaxLocalIdx() const { return m_maxLocalIdx; }

private:
    FuncMetaPtr m_meta;
    vector<map<string, int> > m_blockNames;
    int m_maxLocalIdx;
    int m_curLocalIdx;

    static vector<SymbolTable*> s_tables;
};


#endif
