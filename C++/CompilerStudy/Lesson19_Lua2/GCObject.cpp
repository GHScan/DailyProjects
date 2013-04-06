
#include "pch.h"
#include "GCObject.h"
#include "LuaVM.h"
#include "LuaString.h"

GCObjectManager::GCObjectManager(): m_headObj(NULL) {
}
GCObjectManager::~GCObjectManager() {
    ASSERT(m_headObj == NULL);
}

void GCObjectManager::performFullGC() {
    // TODO
    LuaVM::instance()->getStringPool()->onFullGCEnd(m_headObj);
}
void GCObjectManager::linkObject(GCObject *obj) {
    obj->next = m_headObj;
    m_headObj = obj;
}
