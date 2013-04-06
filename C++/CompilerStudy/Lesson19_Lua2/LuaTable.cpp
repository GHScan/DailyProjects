
#include "pch.h"
#include "LuaTable.h"

LuaTable::LuaTable(): GCObject(OT_Table), m_metaTable(NULL) {
}
LuaTable::~LuaTable() {
}

LuaValue& LuaTable::get(const LuaValue& k, bool raw) {
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_array.size()) return m_array[idx];
    }
    auto iter = m_dict.find(k);
    if (iter != m_dict.end()) return iter->second;

    if (!raw) {
        LuaValue &v = getMeta("__index");
        if (v.isNil());
        else if (v.isTypeOf(LVT_Table)) {
            return v.getTable()->get(k);
        } else if (v.isTypeOf(LVT_Function)) {
            // TODO
        }
    }

    return LuaValue::NIL;
}
void LuaTable::set(const LuaValue& k, const LuaValue& v, bool raw) {
    // TODO

    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_array.size()) m_array[idx] = v;
        else if (idx == (int)m_array.size()) m_array.push_back(v);
        else {}
    }
    m_dict[k] = v;
}

LuaValue& LuaTable::getNext(LuaValue& k) {
    if (k.isNil()) {
        if (!m_array.empty()) {
            k = LuaValue(1);
            return m_array[0];
        }
        if (!m_dict.empty()) {
            k = m_dict.begin()->first;
            return m_dict.begin()->second;
        }
        return LuaValue::NIL;
    }

    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber();
        if (idx >= 0 && idx < (int)m_array.size()) {
            k = LuaValue(idx + 1);
            return m_array[idx];
        }
        else if (idx == (int)m_array.size()) {
            k = m_dict.begin()->first;
            return m_dict.begin()->second;
        }
    }

    auto iter = m_dict.find(k);
    if (iter != m_dict.end()) {
        ++iter;
        if (iter != m_dict.end()) {
            k = iter->first;
            return iter->second;
        }
    }

    k = LuaValue::NIL;
    return LuaValue::NIL;
}

LuaValue& LuaTable::getMeta(const char *metaName) {
    if (m_metaTable == NULL) return LuaValue::NIL;
    LuaValue& v = m_metaTable->get(LuaValue(metaName));
    return v;
}
