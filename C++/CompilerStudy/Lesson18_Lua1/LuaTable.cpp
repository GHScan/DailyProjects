#include "pch.h"
#include "LuaTable.h"

template <class T>
inline void hash_combine(int & seed, const T & v) {
    seed ^= std::hash<T>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

const LuaValue& LuaTable::get(const LuaValue& k) const {
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_vec.size()) return m_vec[idx];
    }
    auto iter = m_hashTable.find(k);
    if (iter == m_hashTable.end()) return LuaValue::NIL;
    return iter->second;
}
void LuaTable::set(const LuaValue& k, const LuaValue& v) {
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.getNumber() - 1;
        if (idx >= 0 && idx < (int)m_vec.size()) {
            m_vec[idx] = v;
            return;
        }
        if (idx == (int)m_vec.size()) {
            m_vec.push_back(v);
            return;
        }
    }
    m_hashTable[k] = v;
    return;
}

const LuaValue& LuaTable::getNext(LuaValue& k) const {
    if (k.isTypeOf(LVT_Nil)) {
        if (!m_vec.empty()) {
            k = LuaValue(1);
            return m_vec.front();
        }
        if (!m_hashTable.empty()) {
            auto iter = m_hashTable.begin();
            k = iter->first;
            return iter->second;
        }
        return LuaValue::NIL;
    }
    else {
        if (k.isTypeOf(LVT_Number)) {
            int idx = (int)k.getNumber() - 1;
            if (idx >= 0 && idx < (int)m_vec.size()) {
                k = LuaValue(idx + 2);
                return m_vec[idx];
            }
            if (idx == (int)m_vec.size() && !m_hashTable.empty()) {
                auto iter = m_hashTable.begin();
                auto iter2 = iter; ++iter2;
                if (iter2 == m_hashTable.end()) k = LuaValue::NIL;
                else k = iter2->first;
                return iter->second;
            }
        }
        auto iter = m_hashTable.find(k);
        if (iter == m_hashTable.end()) {
            k = LuaValue::NIL;
            return LuaValue::NIL;
        } else {
            auto iter2 = iter; ++iter2;
            if (iter2 == m_hashTable.end()) k = LuaValue::NIL;
            else k = iter2->first;
            return iter->second;
        }
    }
}

int LuaTable::getHash() const {
    if (m_hash == 0) {
        // TODO: Uncomment to reach the value semantic
        //hash_combine(m_hash, 0);
        //LuaValue k = LuaValue::NIL;
        //for (;;) {
        //    const LuaValue &v = getNext(k);
        //    if (k.isTypeOf(LVT_Nil)) break;
        //    hash_combine(m_hash, k.getHash());
        //    hash_combine(m_hash, v.getHash());
        //}
        hash_combine(m_hash, this);
    }
    return m_hash;
}


