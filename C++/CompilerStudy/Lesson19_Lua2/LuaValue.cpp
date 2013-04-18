
#include "pch.h"

#include "LuaValue.h"
#include "LuaVM.h"
#include "LuaString.h"
#include "LuaFunction.h"
#include "GCObject.h"
#include "LuaTable.h"
#include "LuaStack.h"

LuaValue::LuaValue(NumberType num): 
    m_type(LVT_Number) {
    m_data.num = num;
}
LuaValue::LuaValue(const char *str):
    m_type(LVT_String) {
    m_data.str = LuaVM::instance()->getStringPool()->createString(str);
}
LuaValue::LuaValue(const char *str, int size):
    m_type(LVT_String) {
    m_data.str = LuaVM::instance()->getStringPool()->createString(str, size);
}
LuaValue::LuaValue(LuaTable* table):
    m_type(LVT_Table) {
    m_data.table = table;
}
LuaValue::LuaValue(Function* func):
    m_type(LVT_Function) {
    m_data.func = func;
}
LuaValue::LuaValue(LuaStack* stack):
    m_type(LVT_Stack) {
    m_data.stack = stack;
}
LuaValue::LuaValue(LightUserData lud):
    m_type(LVT_LightUserData) {
    m_data.lud = lud;
}

LuaValue& LuaValue::operator += (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.num += o.m_data.num;
    return *this;
}
LuaValue& LuaValue::operator -= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.num -= o.m_data.num;
    return *this;
}
LuaValue& LuaValue::operator *= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.num *= o.m_data.num;
    return *this;
}
LuaValue& LuaValue::operator /= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.num /= o.m_data.num;
    return *this;
}
LuaValue& LuaValue::operator %= (const LuaValue& o) {
    ASSERT(m_type == LVT_Number && o.m_type == LVT_Number);
    m_data.num = fmod(m_data.num, o.m_data.num);
    return *this;
}

bool LuaValue::operator == (const LuaValue& o) const {
    if (m_type == o.m_type) {
        switch (m_type) {
        case LVT_Boolean: return m_data.b == o.m_data.b;
        case LVT_Number: return m_data.num == o.m_data.num;
        case LVT_String: return m_data.str == o.m_data.str;
        case LVT_Table: return m_data.table == o.m_data.table;
        case LVT_Function: return m_data.func == o.m_data.func;
        case LVT_Stack: return m_data.stack == o.m_data.stack;
        case LVT_LightUserData: return m_data.lud == o.m_data.lud;
        default: ASSERT(0);
        }
    }
    return false;
}
bool LuaValue::operator < (const LuaValue& o) const {
    if (m_type == o.m_type) {
        switch (m_type) {
        case LVT_Boolean: ASSERT(0);
        case LVT_Number: return m_data.num < o.m_data.num;
        case LVT_String: return m_data.str->isContentLess(*o.m_data.str);
        case LVT_Table: ASSERT(0);
        case LVT_Function: ASSERT(0);
        case LVT_Stack: ASSERT(0);
        case LVT_LightUserData: ASSERT(0);
        default: ASSERT(0);
        }
    } 
    ASSERT(0);
    return false;
}

GCObject* LuaValue::gcAccess() const {
    switch (m_type) {
        case LVT_String: return m_data.str->gcAccess();
        case LVT_Table: return m_data.table->gcAccess();
        case LVT_Function: return m_data.func->gcAccess();
        case LVT_Stack: return m_data.stack->gcAccess();
        default: return NULL;
    }
}

LuaValue LuaValue::NIL;
LuaValue LuaValue::TRUE(LuaValue::fromBoolean(true));
LuaValue LuaValue::FALSE(LuaValue::fromBoolean(false));

LuaValue power(const LuaValue& l, const LuaValue& r) {
    ASSERT(l.isTypeOf(LVT_Number) && r.isTypeOf(LVT_Number));
    return LuaValue(NumberType(::pow(l.getNumber(), r.getNumber())));
}
LuaValue concat(const LuaValue& l, const LuaValue& r) {
    ASSERT(l.isTypeOf(LVT_String) && r.isTypeOf(LVT_String));
    auto str1 = l.getString(), str2 = r.getString();
    int nsize = str1->size() + str2->size();
    vector<char> buf(nsize);
    if (nsize > 0) {
        memcpy(&buf[0], str1->buf(), str1->size());
        memcpy(&buf[0] + str1->size(), str2->buf(), str2->size());
        return LuaValue(&buf[0], nsize);
    } else {
        return LuaValue("", 0);
    }
}
