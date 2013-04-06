
#include "pch.h"
#include "LuaStack.h"
#include "LuaFunction.h"

LuaStackFrame::LuaStackFrame(LuaStack *_stack, Function *_func, int paramBase, int paramCount):
    stack(_stack), func(_func), ip(0), localBase(paramBase + paramCount), tempCount(0), tempExtCount(0){ 
    int localCount = 0, argCount;
    if (func != NULL && func->funcType == Function::FT_Lua) {
        localCount = static_cast<LuaFunction*>(func)->meta->localCount;
        argCount = static_cast<LuaFunction*>(func)->meta->argCount;
    }
    tempBase = localBase + localCount;
    varParamBase = localBase - (paramCount - argCount);
    stack->m_values.resize(tempBase, LuaValue::NIL);
}

//===== LuaStack =====
void LuaStack::pushFrame(Function *func, int paramBase, int paramCount) {
    m_frames.push_back(new LuaStackFrame(this, func, paramBase, paramCount));
}
void LuaStack::popFrame() {
    delete m_frames.back();
    m_frames.pop_back();
}
LuaStack::LuaStack():
    GCObject(OT_Stack) {
    pushFrame(NULL, 0, 0);
}
LuaStack::~LuaStack() {
    popFrame();
    ASSERT(m_frames.empty());
}
