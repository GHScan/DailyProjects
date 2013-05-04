
#include "pch.h"
#include "GCObject.h"
#include "LuaVM.h"
#include "LuaString.h"
#include "LuaTable.h"
#include "LuaStack.h"
#include "LuaFunction.h"

GCObjectManager::GCObjectManager(): m_headObj(NULL), m_objCount(0) {
}
GCObjectManager::~GCObjectManager() {
    ASSERT(m_headObj == NULL);
    ASSERT(m_objCount == 0);
}

void GCObjectManager::performFullGC() {
    vector<GCObject*> unscaned;
    {
        auto vm = LuaVM::instance();
        if (auto gtable = vm->getGlobalTable()) {
            unscaned.push_back(gtable->gcAccess());
        }
        if (auto stack = vm->getCurrentStack()) {
            unscaned.push_back(stack->gcAccess());
        }
    }

    while (!unscaned.empty()) {
        GCObject *o = unscaned.back();
        unscaned.pop_back();
        o->gcState = GCObject::GCS_Scaned;
        switch (o->objType) {
            case GCObject::OT_Table:
                static_cast<LuaTable*>(o)->collectGCObject(unscaned);
                break;
            case GCObject::OT_Function:
                static_cast<Function*>(o)->collectGCObject(unscaned);
                break;
            case GCObject::OT_Stack:
                static_cast<LuaStack*>(o)->collectGCObject(unscaned);
                break;
            case GCObject::OT_String: break;
            default: ASSERT(0);
        }
    }

    {
        GCObject *obj = m_headObj;
        m_headObj = NULL;
        m_objCount = 0;
        while (obj != NULL) {
            GCObject *temp = obj;
            obj = obj->next;
            if (temp->gcState == GCObject::GCS_Unaccess) {
                switch (temp->objType) {
                    case GCObject::OT_Table: 
                        static_cast<LuaTable*>(temp)->destroy();
                        break;
                    case GCObject::OT_String: 
                        static_cast<LuaString*>(temp)->destroy();
                        break;
                    case GCObject::OT_Stack: 
                        static_cast<LuaStack*>(temp)->destroy();
                        break;
                    case GCObject::OT_Function: 
                        static_cast<Function*>(temp)->destroy();
                        break;
                    default: ASSERT(0);
                }
            } else {
                ASSERT(temp->gcState == GCObject::GCS_Scaned);
                ++m_objCount;
                temp->gcState = GCObject::GCS_Unaccess;
                temp->next = m_headObj;
                m_headObj = temp;
            }
        }
    }

    LuaVM::instance()->getStringPool()->onFullGCEnd(m_headObj);
}
void GCObjectManager::linkObject(GCObject *obj) {
    obj->next = m_headObj;
    m_headObj = obj;
    ++m_objCount;
}
