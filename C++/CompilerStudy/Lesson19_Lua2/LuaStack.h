
#ifndef LUA_STACK_H
#define LUA_STACK_H

#include "GCObject.h"
#include "LuaValue.h"

struct LuaFunction;
struct LuaStack;

struct LuaStackFrame {
    LuaStack *stack;
    LuaFunction *func; 
    LuaValue *localPtr, *varParamPtr;
    int ip;
    int retN;
    // TODO: replace it with priority_queue for performance
    multimap<int, pair<LuaFunction*, int> > closures;

    LuaStackFrame(LuaStack *_stack, LuaFunction *_func, int paramBase, int paramCount);
    void setExtCount(int _extCount) { extCount = _extCount;}
    int getExtCount();
private:
    int extCount;
};

struct LuaStack:
    public GCObject {
    static LuaStack* create() {
        return new LuaStack();
    }
    void destroy() {
        delete this;
    }

    LuaStackFrame* topFrame(int idx = 0) { 
        assert(idx <= 0);
        return m_frames[m_frames.size() - 1 + idx];
    }
    void pushFrame(LuaFunction *func, int paramBase, int paramCount);
    void popFrame();
    LuaStackFrame* topFrameOfLevel(int level);

    void collectGCObject(vector<GCObject*>& unscaned);

    vector<LuaValue>& values() { return m_values; }
    void reserveValueSpace(int n) { if ((int)m_values.size() < n) m_values.resize(n, LuaValue::NIL); }

private:
    LuaStack();
    ~LuaStack();

private:
    vector<vector<LuaStackFrame*> > m_framesOfLevel;
    vector<LuaValue> m_values;
    vector<LuaStackFrame*> m_frames;

};

inline int LuaStackFrame::getExtCount() { 
    if (extCount == -1) return 1;
    int r = extCount; 
    extCount = -1; 
    return r; 
}

#endif
