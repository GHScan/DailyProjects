
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
    int tempBase, tempCount;

    LuaStackFrame(LuaStack *_stack, Function *func, int paramBase, int paramCount);
    LuaValue& local(int localIdx);
    LuaValue& topTemp(int idx);
    LuaValue& temp(int tempIdx);
    LuaValue& stackValue(int idx); 
    void pushTemp(const LuaValue& v);
    void pushExtTemp(const LuaValue& v);
    void popTempN(int n);
    void popTemps(int startTempIdx);
    void resizeTemp(int n);
    void resizeTemp2Ext();
private:
    int tempExtCount;
};

struct LuaStack:
    public GCObject {
    static LuaStack* create() {
        return new LuaStack();
    }
    void destroy() {
        delete this;
    }

    LuaStackFrame* topFrame() { return m_frames.back(); }
    LuaStackFrame* topFrame(int idx) { 
        assert(idx <= 0);
        return m_frames[m_frames.size() - 1 + idx];
    }
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
inline LuaValue& LuaStackFrame::topTemp(int idx) {
    assert(idx <= 0);
    return stack->m_values[tempBase + tempCount + idx - 1];
}
inline LuaValue& LuaStackFrame::temp(int tempIdx) { return stack->m_values[tempBase + tempIdx]; }
inline LuaValue& LuaStackFrame::stackValue(int idx) { return stack->m_values[idx]; }
inline void LuaStackFrame::pushTemp(const LuaValue& v) {
    tempExtCount = ++tempCount;
    stack->m_values.resize(tempBase + tempCount);
    stack->m_values.back() = v;
}
inline void LuaStackFrame::pushExtTemp(const LuaValue& v) {
    stack->m_values.resize(tempBase + ++tempExtCount);
    stack->m_values.back() = v;
}
inline void LuaStackFrame::popTempN(int n) {
    popTemps(tempCount - n);
}
inline void LuaStackFrame::popTemps(int startTempIdx) {
    tempExtCount = tempCount = startTempIdx;
    stack->m_values.resize(tempBase + startTempIdx);
}
inline void LuaStackFrame::resizeTemp(int n) {
    tempCount = tempExtCount = n;
    stack->m_values.resize(tempBase + tempCount);
}
inline void LuaStackFrame::resizeTemp2Ext() {
    tempCount = tempExtCount;
    stack->m_values.resize(tempBase + tempCount);
}

#endif
