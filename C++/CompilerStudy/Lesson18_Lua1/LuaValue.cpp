#include "pch.h"
#include "Ast.h"
#include "LuaValue.h"
#include "LuaTable.h"
#include "LuaFunction.h"

LuaValue::LuaValue(const char *str):
    m_type(LVT_String) {
    int len = strlen(str);
    m_data.str = new char[len + 1];
    memcpy(m_data.str, str, len + 1);
}

LuaValue::LuaValue(LuaValueType t, int v):
    m_type(t) {
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
    m_type(LVT_Nil) {
    m_data.n = 0;
    *this = o;
}

LuaValue::LuaValue(LuaValue&& o):
    m_type(LVT_Nil) {
    m_data.n = 0;
    *this = forward<LuaValue>(o);
}
LuaValue& LuaValue::operator = (const LuaValue& o) {
    if (this == &o) return *this;
    this->~LuaValue();
    m_type = o.m_type;
    m_data.n = o.m_data.n;
    switch (o.m_type) {
        case LVT_String: {
                int len = strlen(o.m_data.str);
                m_data.str = new char[len + 1];
                memcpy(m_data.str, o.m_data.str, len + 1);
            }
            break;
        case LVT_Table:
            m_data.table->addRef();
            break;
        case LVT_Function:
            m_data.func->addRef();
            break;
        default:break;
    }
    return *this;
}
LuaValue& LuaValue::operator = (LuaValue&& o) {
    if (this == &o) return *this;
    this->~LuaValue();
    m_type = o.m_type;
    m_data.n = o.m_data.n;
    o.m_type = LVT_Nil;
    o.m_data.n = 0;
    return *this;
}
LuaValue::~LuaValue() {
    switch (m_type) {
        case LVT_String:
            delete[] m_data.str;
            break;
        case LVT_Table:
            m_data.table->releaseRef();
            break;
        case LVT_Function:
            m_data.func->releaseRef();
            break;
        default: break;
    }
    m_type = LVT_Nil;
    m_data.n = 0;
}

bool LuaValue::operator == (const LuaValue& o) const {
    if (m_type == o.m_type) {
        switch (m_type) {
            case LVT_String:
                return strcmp(m_data.str, o.m_data.str) == 0;
            default:
                return m_data.n == o.m_data.n;
        }
    }
    return false;
}

bool LuaValue::operator < (const LuaValue& o) const {
    ASSERT(m_type == o.m_type);
    switch (m_type) {
        case LVT_String:
            return strcmp(m_data.str, o.m_data.str) < 0;
        case LVT_Table:
            ASSERT(0);
        case LVT_Number:
            return m_data.n < o.m_data.n;
        case LVT_Boolean:
            ASSERT(0);
        case LVT_Nil:
            ASSERT(0);
        default: break;
    }
    ASSERT(0);
    return false;
}

LuaValue& LuaValue::operator += (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n += o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator -= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n -= o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator *= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n *= o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator /= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n /= o.m_data.n;
    return *this;
}
LuaValue& LuaValue::operator %= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.n = fmod(m_data.n, o.m_data.n);
    return *this;
}

const LuaValue& LuaValue::get(const LuaValue& k) {
    ASSERT(m_type == LVT_Table);
    return m_data.table->get(k);
}
void LuaValue::set(const LuaValue& k, const LuaValue& v) {
    ASSERT(m_type == LVT_Table);
    m_data.table->set(k, v);
}

int LuaValue::getHash() const {
    switch (m_type) {
        case LVT_Nil: return hash<int>()(0);
        case LVT_Boolean: return hash<bool>()(m_data.b);
        case LVT_Number: return hash<NumberType>()(m_data.n);
        case LVT_String: return hash<const char*>()(m_data.str);
        case LVT_Table: return hash<LuaTable*>()(m_data.table);
        case LVT_Function: return hash<LuaFunction*>()(m_data.func);
        default: break;
    }
    ASSERT(0);
    return 0;
}

string LuaValue::toString() const {
    switch (m_type) {
        case LVT_Nil: return "nil";
        case LVT_Boolean: return m_data.b ? "true" : "false";
        case LVT_Number: return m_data.n == (int)m_data.n ? format("%d", m_data.n) : format("%f", m_data.n);
        case LVT_String: return m_data.str;
        case LVT_Table: return format("table: %p", m_data.table);
        case LVT_Function: return format("function: %p", m_data.func);
        default: break;
    }
    ASSERT(0);
    return "";
}

LuaValue LuaValue::NIL;
LuaValue LuaValue::TRUE(LVT_Boolean, 1);
LuaValue LuaValue::FALSE(LVT_Boolean, 0);
