
#ifndef LUA_TABLE_H
#define LUA_TABLE_H

#include "GCObject.h"
#include "LuaValue.h"

// TODO: add metatable
class LuaTable:
    public GCObject {
public:
    static LuaTable* create() {
        return new LuaTable();
    }

    ~LuaTable();

    LuaValue& get(const LuaValue& k, bool raw = false);
    void set(const LuaValue& k, const LuaValue& v, bool raw = false);

    int size() const { return (int)m_array.size();}

    LuaValue& getNext(LuaValue& k);

    void setMetatable(LuaTable *table) {m_metaTable = table;}

    LuaValue& getMeta(const char *metaName);
private:
    LuaTable();
    LuaTable(const LuaTable&);
    LuaTable& operator = (const LuaTable&);

private:
    vector<LuaValue> m_array;
    unordered_map<LuaValue, LuaValue> m_dict;
    LuaTable *m_metaTable;
};

#endif
