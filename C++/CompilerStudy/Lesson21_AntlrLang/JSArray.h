
#ifndef JS_ARRAY_H
#define JS_ARRAY_H

#include "GCObject.h"
#include "JSValue.h"

struct JSArray:
    public GCObject {
    vector<JSValue> array;

    JSArray(): GCObject(GCT_Array){ 
        GCObjectManager::instance()->link(this);
    }

    void accessGCObjects(vector<GCObject*> &objs) {
        for (auto &value : array) {
            if (auto obj = value.gcAccess()) objs.push_back(obj);
        }
    }
};

#endif
