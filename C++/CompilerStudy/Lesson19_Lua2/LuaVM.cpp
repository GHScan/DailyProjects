
#include "pch.h"

#include "LuaVM.h"
#include "GCObject.h"
#include "LuaString.h"
#include "LuaStack.h"


void LuaVM::create() {
    s_ins = new LuaVM();
}
void LuaVM::destroy() {
    sdelete(s_ins);
}
LuaVM* LuaVM::s_ins;

LuaVM::LuaVM(): 
    m_gcObjMgr(new GCObjectManager),
    m_strPool(new StringPool),
    m_curStack(LuaStack::create()) {
}
LuaVM::~LuaVM() {
    m_curStack = NULL;
    sdelete(m_gcObjMgr);
    sdelete(m_strPool);
}

