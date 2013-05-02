
#include "pch.h"
#include "LuaStack.h"
#include "LuaFunction.h"
#include "LuaVM.h"
#include "GCObject.h"

LuaStackFrame::LuaStackFrame(LuaStack *_stack, LuaFunction *_func, int paramBase, int paramCount) 
    : stack(_stack), func(_func), localPtr(NULL), varParamPtr(NULL), ip(0), retN(0), extCount(-1) {
    assert(paramCount >= func->meta->argCount);
    auto &values = stack->values();
    if (paramCount > func->meta->argCount) {
        auto iter = values.begin() + paramBase;
        std::rotate(iter, iter + func->meta->argCount, iter + paramCount);
    }
    varParamPtr = &values[0] + paramBase;
    localPtr = varParamPtr + (paramCount - func->meta->argCount);
    stack->reserveValueSpace(paramBase + (paramCount - func->meta->argCount) + 
        func->meta->localCount + func->meta->tempCount);
}

//===== LuaStack =====
void LuaStack::pushFrame(LuaFunction *func, int paramBase, int paramCount) {
    m_frames.push_back(new LuaStackFrame(this, func, paramBase, paramCount));
    int level = func->meta->level;
    if ((int)m_framesOfLevel.size() <= level) m_framesOfLevel.resize(level + 1);
    m_framesOfLevel[level].push_back(m_frames.back());
}
void LuaStack::popFrame() {
    auto func = m_frames.back()->func;
    m_framesOfLevel[func->meta->level].pop_back();

    delete m_frames.back();
    m_frames.pop_back();
}
LuaStackFrame* LuaStack::topFrameOfLevel(int level) {
    return m_framesOfLevel[level].back();
}
LuaStack::LuaStack():
    GCObject(OT_Stack) {
    m_values.reserve(32 * 1024); // FIXME
    LuaVM::instance()->getGCObjManager()->linkObject(this);
}
LuaStack::~LuaStack() {
    ASSERT(m_frames.empty());
}

void LuaStack::collectGCObject(vector<GCObject*>& unscaned) {
    for (auto &v : m_values) {
        if (auto p = v.gcAccess()) unscaned.push_back(p);
    }
    for (auto frame : m_frames) {
        if (frame->func == NULL) continue;
        if (auto p = frame->func->gcAccess()) unscaned.push_back(p);
    }
}
