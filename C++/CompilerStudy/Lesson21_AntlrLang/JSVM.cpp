
#include "pch.h"
#include "JSVM.h"

void JSVM::accessGCObjects(vector<GCObject*> &objs) {
    for (auto &kv : m_globals) {
        if (auto obj = kv.first.gcAccess()) objs.push_back(obj);
        if (auto obj = kv.second.gcAccess()) objs.push_back(obj);
    }
    for (auto &value : m_values) {
        if (auto obj = value.gcAccess()) objs.push_back(obj);
    }
}
