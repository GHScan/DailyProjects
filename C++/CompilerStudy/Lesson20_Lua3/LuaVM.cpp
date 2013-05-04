
#include "pch.h"

#include "LuaVM.h"
#include "GCObject.h"
#include "LuaString.h"
#include "LuaStack.h"
#include "LuaTable.h"

void LuaVM::create() {
    ASSERT(s_ins == NULL);
    new LuaVM();
}
void LuaVM::destroy() {
    sdelete(s_ins);
    ASSERT(s_ins == NULL);
}
LuaVM* LuaVM::s_ins;

LuaVM::LuaVM(): 
    m_gcObjMgr(NULL), m_strPool(NULL), m_curStack(NULL), m_gtable(NULL){
    s_ins = this;
    m_gcObjMgr = new GCObjectManager;
    m_strPool = new StringPool;
    m_gtable = LuaTable::create();
    m_curStack = LuaStack::create();
}
LuaVM::~LuaVM() {
    m_gtable = NULL;
    m_curStack = NULL;
    m_gcObjMgr->performFullGC();
    sdelete(m_gcObjMgr);
    sdelete(m_strPool);
    s_ins = NULL;
}

