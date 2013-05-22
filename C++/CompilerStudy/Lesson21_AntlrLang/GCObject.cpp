
#include "pch.h"
#include "JSString.h"
#include "JSVM.h"
#include "JSArray.h"
#include "JSFunction.h"

GCObjectManager::~GCObjectManager() {
    ASSERT(m_head == NULL);
}

void GCObjectManager::link(GCObject *obj) {
    obj->next = m_head;
    m_head = obj;
}
void GCObjectManager::performFullGC() {
    vector<GCObject*> unscans;
    JSVM::instance()->accessGCObjects(unscans);

    while (!unscans.empty()) {
        GCObject *obj = unscans.back();
        unscans.pop_back();
        switch (obj->type) {
            case GCObject::GCT_Array:
                static_cast<JSArray*>(obj)->accessGCObjects(unscans);
            case GCObject::GCT_Function:
                static_cast<JSFunction*>(obj)->accessGCObjects(unscans);
                break;
            default: break;
        }
        obj->state = GCObject::GCS_Scan;
    }

    GCObject *oldHead = m_head;
    m_head = NULL;
    while (oldHead != NULL) {
        GCObject *next = oldHead->next;
        if (oldHead->state == GCObject::GCS_Unaccess) {
            switch (oldHead->type) {
                case GCObject::GCT_Array:
                    delete static_cast<JSArray*>(oldHead);
                    break;
                case GCObject::GCT_String:
                    delete static_cast<JSString*>(oldHead);
                    break;
                case GCObject::GCT_Function:
                    static_cast<Function*>(oldHead)->destroy();
                    break;
                default: ASSERT(0); break;
            }
        } else {
            ASSERT(oldHead->state == GCObject::GCS_Scan);
            oldHead->state = GCObject::GCS_Unaccess;
            oldHead->next = m_head;
            m_head = oldHead;
        }
        oldHead = next;
    }

    JSStringManager::instance()->notifyValidGCObjects(m_head);
}
