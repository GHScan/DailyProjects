
#include "pch.h"

#include "Runtime.h"
#include "LuaTable.h"
#include "LuaFunction.h"

Runtime::Runtime():
    m_gtable(NULL) {
}
Runtime::~Runtime() {
    if (m_gtable) m_gtable->releaseRef();
}
void Runtime::setGlobalTable(LuaTable *t) {
    if (m_gtable != t) {
        if (m_gtable) m_gtable->releaseRef();
        if (t) t->addRef();
        m_gtable = t;
    }
}
StackFrame* Runtime::getFrameByLevel(int level) {
    return m_levelFrames[level].back();
}

void Runtime::pushFrame(LuaFunction *func, int localCount) {
    m_frames.push_back(new StackFrame(func, localCount)); 

    int level = func->getMeta()->level;
    if ((int)m_levelFrames.size() <= level) {
        m_levelFrames.resize(level + 1);
    }
    m_levelFrames[level].push_back(m_frames.back());
}
void Runtime::pushFrame(CFunction *func) {
    m_frames.push_back(new StackFrame(func, 0)); 
}
void Runtime::popFrame() { 
    if (auto func = dynamic_cast<LuaFunction*>(m_frames.back()->getFunc())) {
        m_levelFrames[func->getMeta()->level].pop_back();
    }

    delete m_frames.back();
    m_frames.pop_back(); 
}
