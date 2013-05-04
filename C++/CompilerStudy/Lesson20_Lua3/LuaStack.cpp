
#include "pch.h"
#include "LuaStack.h"
#include "LuaFunction.h"
#include "LuaVM.h"
#include "GCObject.h"

LuaStackFrame::LuaStackFrame(LuaStack *_stack, LuaFunction *_func, int paramBase, int paramCount, int _requireRetN) 
    : stack(_stack), func(_func), localPtr(NULL), varParamPtr(NULL), ip(0), retN(0), extCount(-1), requireRetN(_requireRetN) {
    int argCount = 0, localCount = 0, tempCount = 0;
    if (func != NULL) {
        argCount = func->meta->argCount;
        localCount = func->meta->localCount;
        tempCount = func->meta->tempCount;
    }

    assert(paramCount >= argCount);
    auto &values = stack->values();
    if (paramCount > argCount) {
        auto iter = values.begin() + paramBase;
        std::rotate(iter, iter + argCount, iter + paramCount);
    }
    varParamPtr = values.empty() ? NULL : &values[0] + paramBase;
    localPtr = varParamPtr + (paramCount - argCount);
    stack->reserveValueSpace(paramBase + (paramCount - argCount) + localCount + tempCount);
}

//===== LuaStack =====
void LuaStack::pushFrame(LuaFunction *func, int paramBase, int paramCount, int requireRetN) {
    m_frames.push_back(new LuaStackFrame(this, func, paramBase, paramCount, requireRetN));
    if (func != NULL) {
        int level = func->meta->level;
        if ((int)m_framesOfLevel.size() <= level) m_framesOfLevel.resize(level + 1);
        m_framesOfLevel[level].push_back(m_frames.back());
    }
}
void LuaStack::popFrame() {
    auto func = m_frames.back()->func;
    if (func != NULL) {
        m_framesOfLevel[func->meta->level].pop_back();
    }

    delete m_frames.back();
    m_frames.pop_back();
}
LuaStackFrame* LuaStack::topFrameOfLevel(int level) {
    return m_framesOfLevel[level].back();
}
LuaStack::LuaStack():
    GCObject(OT_Stack) {
    pushFrame(NULL, 0, 0, false);
    m_values.reserve(32 * 1024); // FIXME
    LuaVM::instance()->getGCObjManager()->linkObject(this);
}
LuaStack::~LuaStack() {
    popFrame();
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
