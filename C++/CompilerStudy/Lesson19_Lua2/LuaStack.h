
#ifndef LUA_STACK_H
#define LUA_STACK_H

#include "GCObject.h"
#include "LuaValue.h"

struct Function;
class LuaStack;

struct LuaStackFrame {
    LuaStack *stack;
    Function *func; 
    int ip;
    int varParamBase, localBase;
    int tempBase, tempCount, tempExtCount;

    LuaStackFrame(LuaStack *_stack, Function *func, int paramBase, int paramCount);
    LuaValue& local(int localIdx);
    LuaValue& temp(int tempIdx);
    void pushTemp(const LuaValue& v);
    void pushExtTemp(const LuaValue& v);
    void popTemps(int startTempIdx);
};

struct LuaStack:
    // TODO: should it derived from gcobject ???
    public GCObject {
    static LuaStack* create() {
        return new LuaStack();
    }
    void destroy() {
        delete this;
    }

    LuaStackFrame* topFrame() { return m_frames.back(); }
    void pushFrame(Function *func, int paramBase, int paramCount);
    void popFrame();

    void collectGCObject(vector<GCObject*>& unscaned);

    vector<LuaValue> m_values;
    vector<LuaStackFrame*> m_frames;

private:
    LuaStack();
    ~LuaStack();
};

inline LuaValue& LuaStackFrame::local(int localIdx) { return stack->m_values[localBase + localIdx];}
inline LuaValue& LuaStackFrame::temp(int tempIdx) { return stack->m_values[tempBase + tempIdx]; }
inline void LuaStackFrame::pushTemp(const LuaValue& v) {
    tempExtCount = ++tempCount;
    stack->m_values.resize(tempBase + tempCount);
    stack->m_values.back() = v;
}
inline void LuaStackFrame::pushExtTemp(const LuaValue& v) {
    stack->m_values.resize(tempBase + ++tempExtCount);
    stack->m_values.back() = v;
}
inline void LuaStackFrame::popTemps(int startTempIdx) {
    tempExtCount = tempCount = startTempIdx;
    stack->m_values.resize(tempBase + startTempIdx);
}

#endif
