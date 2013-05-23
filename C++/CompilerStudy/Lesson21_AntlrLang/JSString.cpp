
#include "pch.h"
#include "JSString.h"

JSStringManager* JSStringManager::s_ins;
JSString* JSStringManager::get(const char *str) {
    JSString *r = NULL;
    JSString test;
    test.attach(str);
    auto iter = m_strs.find(&test);
    if (iter != m_strs.end()) {
        r = *iter;
    } else {
        r = new JSString(str);
        m_strs.insert(r);
    }
    test.detach();
    return r;
}

void JSStringManager::notifyValidGCObjects(GCObject *obj) {
    m_strs.clear();
    while (obj != NULL) {
        if (obj->type == GCObject::GCT_String) {
            m_strs.insert(static_cast<JSString*>(obj));
        }
        obj = obj->next;
    }
}
