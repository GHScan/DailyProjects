#include "pch.h"
#include "LuaValue.h"

//======== LuaTable ============

const LuaValue& LuaTable::get(const LuaValue& k) const
{
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.toNumber() - 1;
        if (idx >= 0 && idx < (int)m_vec.size()) return m_vec[idx];
    }
    auto iter = m_hashTable.find(k);
    if (iter == m_hashTable.end()) return LuaValue::NIL;
    return iter->second;
}
void LuaTable::set(const LuaValue& k, const LuaValue& v)
{
    if (k.isTypeOf(LVT_Number)) {
        int idx = (int)k.toNumber() - 1;
        if (idx >= 0 && idx < (int)m_vec.size()) {
            m_vec[idx] = v;
            return;
        }
    }
    m_hashTable[k] = v;
    return;
}

const LuaValue& LuaTable::getNext(LuaValue& k) const
{
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
            int idx = (int)k.toNumber();
            if (idx >= 0 && idx < (int)m_vec.size()) {
                k = LuaValue(idx + 1);
                return m_vec[idx];
            }
            if (idx == (int)m_vec.size() && !m_hashTable.empty()) {
                auto iter = m_hashTable.begin();
                k = iter->first;
                return iter->second;
            }
        }
        auto iter = m_hashTable.find(k);
        if (iter == m_hashTable.end() || ++iter == m_hashTable.end()) {
            k = LuaValue::NIL;
            return LuaValue::NIL;
        }
        k = iter->first;
        return iter->second;
    }
}

int LuaTable::getHash() const
{
    if (m_hash == 0) {
        hash_combine(m_hash, 0);
        LuaValue k = LuaValue::NIL;
        for (;;) {
            const LuaValue &v = getNext(k);
            if (k.isTypeOf(LVT_Nil)) break;
            hash_combine(m_hash, k.getHash());
            hash_combine(m_hash, v.getHash());
        }
    }
    return m_hash;
}

//======== LuaValue ============

LuaValue::LuaValue(const char *str):
    m_type(LVT_String)
{
    int len = strlen(str);
    m_data.str = new char[len + 1];
    memcpy(m_data.str, str, len + 1);
}

LuaValue::LuaValue(LuaValueType t, int v):
    m_type(t)
{
    m_data.n = v;
    if (m_data.n == 0) {
        switch (m_type) {
            case LVT_String:
                m_data.str = new char[1];
                m_data.str[0] = 0;
                break;
            case LVT_Table:
                m_data.table = LuaTable::create();
                break;
            default: break;
        }
    }
}

LuaValue::LuaValue(const LuaValue& o):
    m_type(LVT_Nil)
{
    m_data.n = 0;
    *this = o;
}

LuaValue::LuaValue(LuaValue&& o):
    m_type(LVT_Nil)
{
    m_data.n = 0;
    *this = forward<LuaValue&&>(o);
}
LuaValue& LuaValue::operator = (const LuaValue& o)
{
    if (this == &o) return *this;
    this->~LuaValue();
    m_type = o.m_type;
    m_data.n = o.m_data.n;
    switch (o.m_type) {
        case LVT_String: 
            {
                int len = strlen(o.m_data.str);
                m_data.str = new char[len + 1];
                memcpy(m_data.str, o.m_data.str, len + 1);
            }
            break;
        case LVT_Table:
            m_data.table->addRef();
            break;
        default:break;
    }
    return *this;
}
LuaValue& LuaValue::operator = (LuaValue&& o)
{
    if (this == &o) return *this;
    this->~LuaValue();
    m_type = o.m_type;
    m_data.n = o.m_data.n;
    o.m_type = LVT_Nil;
    o.m_data.n = 0;
    return *this;
}
LuaValue::~LuaValue()
{
    switch (m_type) {
        case LVT_String:
            delete[] m_data.str;
            break;
        case LVT_Table:
            m_data.table->releaseRef();
            break;
        default: break;
    }
    m_data.n = 0;
}

bool LuaValue::operator == (const LuaValue& o) const
{
    if (m_type == o.m_type) {
        switch (m_type) {
            case LVT_String:
                return strcmp(m_data.str, o.m_data.str) == 0;
            case LVT_Table:
                return *m_data.table == *o.m_data.table;
            default:
                return m_data.n == o.m_data.n;
        }
    }
    return false;
}

bool LuaValue::operator < (const LuaValue& o) const
{
    ASSERT(m_type == o.m_type);
    switch (m_type) {
        case LVT_String:
            return strcmp(m_data.str, o.m_data.str) < 0;
        case LVT_Table:
            ASSERT(0);
        case LVT_Number:
            return m_data.n < o.m_data.n;
        case LVT_Boolean:
            return m_data.b < o.m_data.b;
        case LVT_Nil:
            return false;
        default: break;
    }
    ASSERT(0);
    return false;
}

LuaValue& LuaValue::operator += (const LuaValue& o)
{
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n += o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator -= (const LuaValue& o)
{
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n -= o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator *= (const LuaValue& o)
{
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n *= o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator /= (const LuaValue& o)
{
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n /= o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator %= (const LuaValue& o)
{
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n = fmod(m_data.n, o.m_data.n);
    return *this;
}

const LuaValue& LuaValue::get(const LuaValue& k)
{
    ASSERT(m_type == LVT_Table);
    return m_data.table->get(k);
}
void LuaValue::set(const LuaValue& k, const LuaValue& v)
{
    ASSERT(m_type == LVT_Table);
    m_data.table->set(k, v);
}

int LuaValue::getHash() const
{
    switch (m_type) {
        case LVT_Nil: return hash<int>()(0);
        case LVT_Boolean: return hash<bool>()(m_data.b);
        case LVT_Number: return hash<NumberType>()(m_data.n);
        case LVT_String: return hash<const char*>()(m_data.str);
        case LVT_Table: return m_data.table->getHash();
        default: break;
    }
    ASSERT(0);
    return 0;
}

LuaValue LuaValue::NIL;
LuaValue LuaValue::TRUE(LVT_Boolean, 1);
LuaValue LuaValue::FALSE(LVT_Boolean, 0);
