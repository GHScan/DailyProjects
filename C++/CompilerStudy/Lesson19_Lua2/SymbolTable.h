
#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

struct LuaFunctionMeta;

class SymbolTable { public:
    static SymbolTable* top(){ return s_stack.top();}
    static void push(LuaFunctionMeta* meta);
    static void pop();

public:
    void declareLocal(const string& name);
    int getLocalIdx(const string& name) const;
    bool getUpValue(const string& name, pair<int, int>& uvInfo);

    void pushBlock();
    void popBlock();

    LuaFunctionMeta* meta() { return m_meta; }
private:
    SymbolTable(LuaFunctionMeta* meta, SymbolTable* prev, int level);
    ~SymbolTable();

private:
    int m_lastLocalIdx;
    vector<map<string, int> > m_blocks;
    LuaFunctionMeta *m_meta;
    SymbolTable *m_prev;

private:
    static stack<SymbolTable*> s_stack;
};

#endif
