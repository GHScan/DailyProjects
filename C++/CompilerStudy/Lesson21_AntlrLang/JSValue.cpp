
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

JSValue JSValue::NIL;
JSValue JSValue::TRUE(fromBoolean(true));
JSValue JSValue::FALSE(fromBoolean(false));
