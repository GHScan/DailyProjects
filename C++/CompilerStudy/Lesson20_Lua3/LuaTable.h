
#ifndef LUA_TABLE_H
#define LUA_TABLE_H

#include "GCObject.h"
#include "LuaValue.h"

struct LuaStackFrame;

class LuaTable:
    public GCObject {
public:
    static LuaTable* create() { return new LuaTable(); }
    void destroy() { delete this; }

    LuaValue get(const LuaValue& k, bool raw = false);
    void set(const LuaValue& k, const LuaValue& v, bool raw = false);

    void arrayInsert(int off, const LuaValue& v);
    LuaValue arrayRemove(int off);

    int size() const { return (int)m_array.size();}

    LuaValue& getNext(LuaValue& k);
    LuaValue& getINext(LuaValue& k);

    LuaTable* getMetatable() const { return m_metaTable; }
    void setMetatable(LuaTable *table) {m_metaTable = table;}

    LuaValue getMeta(const char *metaName);

    void sort();
    void sort(const LuaValue& cmp);

    friend LuaValue meta_add(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_sub(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_mul(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_div(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_mod(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_pow(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_concat(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_eq(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_lt(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_le(LuaTable *table, const LuaValue& v);
    friend LuaValue meta_unm(LuaTable *table);
    friend void meta_call(LuaTable *table, LuaStackFrame* frame, int tableIdx, int paramCount, int requireRetN);

    void collectGCObject(vector<GCObject*>& unscaned);
private:
    LuaTable();
    LuaTable(const LuaTable&);
    LuaTable& operator = (const LuaTable&);
    ~LuaTable();

private:
    vector<LuaValue> m_array;
    unordered_map<LuaValue, LuaValue> m_dict;
    LuaTable *m_metaTable;
};

#endif
