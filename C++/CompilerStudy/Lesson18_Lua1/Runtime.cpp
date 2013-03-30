
#include "pch.h"

#include "Runtime.h"
#include "LuaTable.h"

Runtime::Runtime():
    m_gtable(LuaTable::create()) {

}
Runtime::~Runtime() {
    m_gtable->releaseRef();
}
void Runtime::setGlobalTable(LuaTable *t) {
    if (m_gtable != t) {
        m_gtable->releaseRef();
        t->addRef();
        m_gtable = t;
    }
}
