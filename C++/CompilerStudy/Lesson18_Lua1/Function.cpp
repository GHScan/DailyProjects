
#include "pch.h"

#include "Function.h"
#include "Runtime.h"
#include "LuaTable.h"

IFunction::IFunction(): m_refCount(1), m_fenv(NULL) {
}
IFunction::~IFunction() {
    if (m_fenv != NULL) m_fenv->releaseRef();
}
LuaTable* IFunction::getfenv() {
    return m_fenv != NULL ? m_fenv : Runtime::instance()->getGlobalTable();
}
void IFunction::setfenv(LuaTable* env) {
    if (m_fenv != env) {
        if (m_fenv != NULL) m_fenv->releaseRef();
        if(env != NULL) env->addRef();
        m_fenv = env;
    }
}

void CFunction::call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
    Runtime::instance()->pushFrame(this);
    m_func(args, rets);
    Runtime::instance()->popFrame();
}
bool CFunction::equal(IFunction *o) {
    if (auto p = dynamic_cast<CFunction*>(o)) {
        return m_func == p->m_func;
    }
    return false;
}
