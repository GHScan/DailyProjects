
#include "pch.h"

#include "Runtime.h"
#include "LuaTable.h"
#include "LuaFunction.h"

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
StackFrame* Runtime::getFrameByLevel(int level) {
    StackFrame *f = NULL;
    for (int i = m_frames.size() - 1; i >= 0; --i) {
        if (auto func = dynamic_cast<LuaFunction*>(m_frames[i].getFunc())) {
            if (func->getMeta()->level == level) {
                f = &m_frames[i];
                break;
            }
        }
    }
    ASSERT(f != NULL);
    return f;
}
