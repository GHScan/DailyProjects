
#ifndef LUA_TABLE_H
#define LUA_TABLE_H

#include "LuaValue.h"

class LuaValue;

class LuaTable
{
public:
    static LuaTable* create()
    {
        return new LuaTable();
    }

    const LuaValue& get(const LuaValue& k) const;
    void set(const LuaValue& k, const LuaValue& v);

    int size() const { return (int)m_vec.size();}

    const LuaValue& getNext(LuaValue& k) const;

    bool operator == (const LuaTable& o) const
    {
        // TODO: Uncomment to reach the value semantic
        //return m_vec == o.m_vec && m_hashTable == o.m_hashTable;
        return this == &o;
    }
    bool operator != (const LuaTable& o) const { return !(*this == o); }

    int getHash() const;

    int getRefCount() const { return m_refCount;}
    int addRef() { return ++m_refCount;}
    int releaseRef() 
    {
        int r = --m_refCount;
        if (r == 0) delete this;
        return r;
    }

private:
    LuaTable(): m_hash(0), m_refCount(1){}
    LuaTable(const LuaTable&);
    LuaTable& operator = (const LuaTable&);

private:
    vector<LuaValue> m_vec;
    unordered_map<LuaValue, LuaValue> m_hashTable;
    mutable int m_hash;
    int m_refCount;
};

#endif
