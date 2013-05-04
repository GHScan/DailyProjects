
#include "pch.h"

#include "LuaValue.h"
#include "LuaVM.h"
#include "LuaString.h"
#include "LuaFunction.h"
#include "GCObject.h"
#include "LuaTable.h"
#include "LuaStack.h"

LuaValue::LuaValue(const char *str):
    m_type(LVT_String) {
    m_data.str = LuaVM::instance()->getStringPool()->createString(str);
}
LuaValue::LuaValue(const char *str, int size):
    m_type(LVT_String) {
    m_data.str = LuaVM::instance()->getStringPool()->createString(str, size);
}

void LuaValue::_lessFrom(const LuaValue& l, const LuaValue& r) {
    if (l.m_type == r.m_type) {
        m_type = LVT_Boolean;
        switch (l.m_type) {
        case LVT_String: m_data.b = l.m_data.str->isContentLess(*r.m_data.str); break;
        case LVT_Table: *this = meta_lt(l.m_data.table, r); break;
        default: ASSERT(0);
        }
    } else {
        ASSERT(0);
    }
}
void LuaValue::_lessEqFrom(const LuaValue& l, const LuaValue& r) {
    if (l.m_type == r.m_type) {
        m_type = LVT_Boolean;
        switch (l.m_type) {
        case LVT_String: m_data.b = l.m_data.str == r.m_data.str || l.m_data.str->isContentLess(*r.m_data.str); break;
        case LVT_Table: *this = meta_le(l.m_data.table, r); break;
        default: ASSERT(0);
        }
    } else {
        ASSERT(0);
    }
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

string LuaValue::toString() const {
    switch (m_type) {
    case LVT_Nil: return "nil";
    case LVT_Boolean: return m_data.b ? "true" : "false";
    case LVT_Number: return m_data.num == int(m_data.num) ? format("%d", int(m_data.num)) : format("%f", m_data.num);
    case LVT_String: return m_data.str->buf();
    case LVT_Table: return format("table: %p", m_data.table);
    case LVT_Function: return format("function: %p", m_data.func);
    case LVT_Stack: return format("thread: %p", m_data.stack);
    case LVT_LightUserData: return format("lightuserdata: %p", m_data.lud);
    default: ASSERT(0);
    }
}

int LuaValue::getSize() const {
    switch (m_type) {
        case LVT_String: return m_data.str->size();
        case LVT_Table: return m_data.table->size();
        default: ASSERT(0);
    }
    return 0;
}

LuaValue LuaValue::NIL;
LuaValue LuaValue::TRUE(LuaValue::fromBoolean(true));
LuaValue LuaValue::FALSE(LuaValue::fromBoolean(false));

void LuaValue::concatFrom(const LuaValue& l, const LuaValue& r) {
    if (l.isTypeOf(LVT_String)) {
        auto str1 = l.getString(), str2 = r.getString();
        int nsize = str1->size() + str2->size();
        vector<char> buf(nsize);
        if (nsize > 0) {
            memcpy(&buf[0], str1->buf(), str1->size());
            memcpy(&buf[0] + str1->size(), str2->buf(), str2->size());
            *this = LuaValue(&buf[0], nsize);
        } else {
            *this = LuaValue("", 0);
        }
    } else {
        *this = meta_concat(l.getTable(), r);
    }
}
