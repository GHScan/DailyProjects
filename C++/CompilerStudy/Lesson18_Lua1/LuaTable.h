
#ifndef LUA_TABLE_H
#define LUA_TABLE_H

#include "LuaValue.h"

class LuaValue;

class LuaTable {
public:
    static LuaTable* create() {
        return new LuaTable();
    }

    LuaValue get(const LuaValue& k, bool raw = false) const;
    void set(const LuaValue& k, const LuaValue& v, bool raw = false);
    bool hasKey(const LuaValue& k) const;

    void arrayInsert(int off, const LuaValue& v);
    LuaValue arrayRemove(int off);

    int size() const { return (int)m_vec.size();}

    void sort();
    void sort(const LuaValue& cmp);

    const LuaValue& getNext(LuaValue& k) const;
    const LuaValue& getINext(LuaValue& k) const;

    void setMetaTable(LuaTable *t);
    LuaTable* getMetaTable() { return m_metaTable;}

    bool hasMeta(const char* metaName) const;
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

    int getRefCount() const { return m_refCount;}
    int addRef() const { return ++m_refCount;}
    int releaseRef() const ;
private:
    LuaTable(const LuaTable&);
    LuaTable& operator = (const LuaTable&);
    LuaTable();
    ~LuaTable();

private:
    vector<LuaValue> m_vec;
    unordered_map<LuaValue, LuaValue> m_hashTable;
    mutable int m_refCount;
    LuaTable *m_metaTable;
};

#endif
