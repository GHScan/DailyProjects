#include "pch.h"
#include "Ast.h"
#include "LuaValue.h"
#include "LuaTable.h"
#include "LuaFunction.h"

LuaValue::LuaValue(const string& str):
    m_type(LVT_String) {
    int len = (int)str.size();
    m_data.str = (char*)malloc(len + 1);
    memcpy(m_data.str, str.c_str(), len + 1);
}

LuaValue::LuaValue(IFunction *func):
    m_type(LVT_Function) {
    m_data.func = func;
}

LuaValue::LuaValue(LuaTable *table): 
    m_type(LVT_Table) {
    m_data.table = table;
}

LuaValue::LuaValue(bool b):
    m_type(LVT_Boolean) {
    m_data.b = b;
}

LuaValue::LuaValue(LightUserData lud): 
    m_type(LVT_LightUserData) {
    m_data.lud = lud;
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
    m_data = o.m_data;
    switch (o.m_type) {
        case LVT_String: {
                int len = (int)strlen(o.m_data.str);
                m_data.str = (char*)malloc(len + 1);
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
    m_data = o.m_data;
    o.m_type = LVT_Nil;
    o.m_data.n = 0;
    return *this;
}
LuaValue::~LuaValue() {
    switch (m_type) {
        case LVT_String:
            free(m_data.str);
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
            case LVT_Nil: return true;
            case LVT_Boolean: return m_data.b == o.m_data.b;
            case LVT_Number: return m_data.n == o.m_data.n;
            case LVT_String: return strcmp(m_data.str, o.m_data.str) == 0;
            case LVT_Table: {
                    if (m_data.table == o.m_data.table) return true;
                    if (m_data.table->hasMeta("__eq")) return m_data.table->meta_eq(o).getBoolean();
                    return false;
                }
            case LVT_Function: return m_data.func->equal(o.m_data.func);
            case LVT_LightUserData: return m_data.lud == o.m_data.lud;
            default: ASSERT(0);
        }
    }
    return false;
}

bool LuaValue::operator <= (const LuaValue& o) const {
    if (isTypeOf(LVT_Table) && o.isTypeOf(LVT_Table)) {
        return m_data.table->meta_le(o).getBoolean();
    }
    return !(o < *this);
}

bool LuaValue::operator < (const LuaValue& o) const {
    ASSERT(m_type == o.m_type);
    switch (m_type) {
        case LVT_String:
            return strcmp(m_data.str, o.m_data.str) < 0;
        case LVT_Table:
            return m_data.table->meta_lt(o).getBoolean();
        case LVT_Number:
            return m_data.n < o.m_data.n;
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
LuaValue LuaValue::power(const LuaValue& o) const {
    if (m_type == LVT_Number && o.m_type == LVT_Number) {
        return LuaValue(::pow(m_data.n, o.m_data.n));
    } else return getTable()->meta_pow(o);
}
LuaValue LuaValue::concat(const LuaValue& o) const {
    if (m_type == LVT_String && o.m_type == LVT_String) {
        int len1 = (int)strlen(m_data.str), len2 = (int)strlen(o.m_data.str);
        char *p = (char*)malloc(len1 + len2 + 1);
        memcpy(p, m_data.str, len1);
        memcpy(p + len1, o.m_data.str, len2 + 1);
        // TODO: performance
        return LuaValue(string(p));
    } else return getTable()->meta_concat(o);
}
int LuaValue::getSize() const {
    if (m_type == LVT_String) {
        return (int)strlen(m_data.str);
    } else if (m_type == LVT_Table) {
        return m_data.table->size();
    } else ASSERT(0);
    return 0;
}

int LuaValue::getHash() const {
    switch (m_type) {
        case LVT_Nil: return (int)hash<int>()(0);
        case LVT_Boolean: return (int)hash<bool>()(m_data.b);
        case LVT_Number: return (int)hash<NumberType>()(m_data.n);
        case LVT_String: return (int)hash<string>()(m_data.str);
        case LVT_Table: return (int)hash<LuaTable*>()(m_data.table);
        case LVT_Function: return (int)hash<IFunction*>()(m_data.func);
        case LVT_LightUserData: return (int)hash<LightUserData>()(m_data.lud);
        default: break;
    }
    ASSERT(0);
    return 0;
}

string LuaValue::toString() const {
    switch (m_type) {
        case LVT_Nil: return "nil";
        case LVT_Boolean: return m_data.b ? "true" : "false";
        case LVT_Number: return m_data.n == (int)m_data.n ? format("%d", (int)m_data.n) : format("%f", m_data.n);
        case LVT_String: return m_data.str;
        case LVT_Table: return format("table: %p", m_data.table);
        case LVT_Function: return format("function: %p", m_data.func);
        case LVT_LightUserData: return format("lightuserdata: %p", m_data.lud);
        default: break;
    }
    ASSERT(0);
    return "";
}

LuaValue LuaValue::NIL;
LuaValue LuaValue::TRUE(true);
LuaValue LuaValue::FALSE(false);

LuaValue operator + (const LuaValue& a, const LuaValue &b) {
    if (a.isTypeOf(LVT_Table)) return a.getTable()->meta_add(b);
    return LuaValue(a) += b;
}
LuaValue operator - (const LuaValue& a, const LuaValue &b) {
    if (a.isTypeOf(LVT_Table)) return a.getTable()->meta_sub(b);
    return LuaValue(a) -= b;
}
LuaValue operator * (const LuaValue& a, const LuaValue &b) {
    if (a.isTypeOf(LVT_Table)) return a.getTable()->meta_mul(b);
    return LuaValue(a) *= b;
}
LuaValue operator / (const LuaValue& a, const LuaValue &b) {
    if (a.isTypeOf(LVT_Table)) return a.getTable()->meta_div(b);
    return LuaValue(a) /= b;
}
LuaValue operator % (const LuaValue& a, const LuaValue &b) {
    if (a.isTypeOf(LVT_Table)) return a.getTable()->meta_mod(b);
    return LuaValue(a) %= b;
}
