#include "pch.h"

#include "LuaTable.h"
#include "Function.h"

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
    if (v.isTypeOf(LVT_Nil)) {
        m_hashTable.erase(k);
    } else m_hashTable[k] = v;
    return;
}
void LuaTable::arrayInsert(int off, const LuaValue& v) {
    ASSERT(off >= 0 && off <= (int)m_vec.size());
    m_vec.insert(m_vec.begin() + off, v);
}
LuaValue LuaTable::arrayRemove(int off) {
    ASSERT(off >= 0 && off < (int)m_vec.size());
    LuaValue r(m_vec[off]);
    m_vec.erase(m_vec.begin() + off);
    return r;
}

const LuaValue& LuaTable::getNext(LuaValue& k) const {
    if (k.isTypeOf(LVT_Nil)) {
        if (!m_vec.empty()) {
            k = LuaValue(NumberType(1));
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
            int idx = (int)k.getNumber();
            if (idx >= 0 && idx < (int)m_vec.size()) {
                k = LuaValue(NumberType(idx + 1));
                return m_vec[idx];
            }
            if (idx == (int)m_vec.size() && !m_hashTable.empty()) {
                auto iter = m_hashTable.begin();
                k = iter->first;
                return iter->second;
            }
        }
        auto iter = m_hashTable.find(k);
        if (iter == m_hashTable.end()) {
            k = LuaValue::NIL;
            return LuaValue::NIL;
        } else {
            auto iter2 = iter; ++iter2;
            if (iter2 == m_hashTable.end()) {
                k = LuaValue::NIL;
                return LuaValue::NIL;
            }
            else {
                k = iter2->first;
                return iter2->second;
            }
        }
    }
}
const LuaValue& LuaTable::getINext(LuaValue& k) const {
    if (k.isTypeOf(LVT_Nil)) {
        if (!m_vec.empty()) {
            k = LuaValue(NumberType(1));
            return m_vec.front();
        }
        return LuaValue::NIL;
    }
    else {
        int idx = (int)k.getNumber();
        if (idx >= 0 && idx < (int)m_vec.size()) {
            k = LuaValue(NumberType(idx + 1));
            return m_vec[idx];
        }
        k = LuaValue::NIL;
        return LuaValue::NIL;
    }
}
void LuaTable::sort() {
    std::sort(m_vec.begin(), m_vec.end());
}
void LuaTable::sort(const LuaValue& cmp) {
    auto func = cmp.getFunction();
    vector<LuaValue> args, rets;
    std::sort(m_vec.begin(), m_vec.end(), [func, &args, &rets](const LuaValue& a, const LuaValue& b){
        args.push_back(a); args.push_back(b);
        func->call(args, rets);
        bool r = rets[0].getBoolean(); 
        args.clear(); rets.clear();
        return r;
    });
}

LuaValue LuaValue::NIL;
LuaValue LuaValue::TRUE(true);
LuaValue LuaValue::FALSE(false);
