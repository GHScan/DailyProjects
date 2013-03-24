
#include "pch.h"

#include "Runtime.h"
#include "LuaTable.h"

Runtime::Runtime():
    m_gtable(LuaTable::create()) {

}
Runtime::~Runtime() {
    m_gtable->releaseRef();
}
