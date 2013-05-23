
#include "pch.h"
#include "JSValue.h"
#include "JSString.h"
#include "JSArray.h"
#include "JSFunction.h"

GCObject* JSValue::gcAccess() const {
    switch (type) {
        case JSVT_String: return data.str->gcAccess();
        case JSVT_Array: return data.array->gcAccess();
        case JSVT_Function: return data.func->gcAccess();
        default: return NULL;
    }
}

JSValue JSValue::fromString(const char *str) {
    JSValue r;
    r.type = JSVT_String;
    r.data.str = JSStringManager::instance()->get(str); 
    return r;
}

string JSValue::toString() const {
    switch (type) {
        case JSVT_Nil: return "nil";
        case JSVT_Boolean: return data.b ? "true" : "false";
        case JSVT_Number: return (int)data.num == data.num ? format("%d", (int)data.num) : format("%lf", data.num);
        case JSVT_String: return data.str->buf;
        case JSVT_Array: return format("[array %p]", data.array);
        case JSVT_Function: return format("[function %p]", data.func);
        default: ASSERT(0); return "";
    }
}

JSValue JSValue::NIL;
JSValue JSValue::TRUE(fromBoolean(true));
JSValue JSValue::FALSE(fromBoolean(false));
