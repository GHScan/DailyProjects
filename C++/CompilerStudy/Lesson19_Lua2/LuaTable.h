
#ifndef LUA_TABLE_H
#define LUA_TABLE_H

#include "GCObject.h"
#include "LuaValue.h"

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

    void setMetatable(LuaTable *table) {m_metaTable = table;}

    LuaValue getMeta(const char *metaName);

    void sort();
    void sort(LuaValue& cmp);

    // TODO: add metatable support 
    LuaValue meta_add(const LuaValue& v);
    LuaValue meta_sub(const LuaValue& v);
    LuaValue meta_mul(const LuaValue& v);
    LuaValue meta_div(const LuaValue& v);
    LuaValue meta_mod(const LuaValue& v);
    LuaValue meta_pow(const LuaValue& v);
    LuaValue meta_concat(const LuaValue& v);
    LuaValue meta_eq(const LuaValue& v);
    LuaValue meta_lt(const LuaValue& v);
    LuaValue meta_le(const LuaValue& v);
    LuaValue meta_unm();
    void meta_call(const vector<LuaValue>& args, vector<LuaValue>& rets);

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
