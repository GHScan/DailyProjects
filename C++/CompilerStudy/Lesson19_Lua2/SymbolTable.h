
#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

struct LuaFunctionMeta;
typedef shared_ptr<LuaFunctionMeta> LuaFunctionMetaPtr;

class SymbolTable { public:
    static SymbolTable* top(){ return s_stack.top();}
    static void push(const LuaFunctionMetaPtr& meta);
    static void pop();

public:
    void declareLocal(const string& name);
    int genInternalLocal(const string& name);
    int getLocalIdx(const string& name) const;
    int getUpValueIdx(const string& name);

    void pushBlock();
    void popBlock();

    LuaFunctionMetaPtr& meta() { return m_meta; }
private:
    SymbolTable(const LuaFunctionMetaPtr &meta, SymbolTable* prev, int level);
    ~SymbolTable();

private:
    int m_lastLocalIdx;
    vector<map<string, int> > m_blocks;
    map<string, int> m_upValues;
    LuaFunctionMetaPtr m_meta;
    SymbolTable *m_prev;

private:
    static stack<SymbolTable*> s_stack;
};

#endif
