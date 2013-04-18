
#include "pch.h"

#include "LuaVM.h"
#include "GCObject.h"
#include "LuaString.h"
#include "LuaStack.h"

void LuaVM::create() {
    ASSERT(s_ins == NULL);
    s_ins = new LuaVM();
}
void LuaVM::destroy() {
    sdelete(s_ins);
}
LuaVM* LuaVM::s_ins;

LuaVM::LuaVM(): 
    m_gcObjMgr(new GCObjectManager),
    m_strPool(new StringPool),
    m_curStack(LuaStack::create()),
    m_gtable(NULL){
}
LuaVM::~LuaVM() {
    m_gtable = NULL;
    m_curStack = NULL;
    sdelete(m_gcObjMgr);
    sdelete(m_strPool);
}

