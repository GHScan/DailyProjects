
#ifndef RUNTIME_H
#define RUNTIME_H

#include "LuaValue.h"

class LuaTable;
struct IFunction;

class StackFrame {
public:
    StackFrame(IFunction* func, int localCount): 
        m_func(func) {
        m_locals.resize(localCount);
    }
    IFunction* getFunc() { return m_func;}
    vector<LuaValue>& locals() { return m_locals;}
    
private:
    vector<LuaValue> m_locals;
    IFunction *m_func;
};

class Runtime {
public:
    static Runtime* instance() {
        static Runtime s_ins;
        return &s_ins;
    }

    LuaTable* getGlobalTable() { return m_gtable; } 
    void setGlobalTable(LuaTable *t);

    StackFrame* getFrame(int off) { 
        ASSERT(off < 0);
        return &m_frames[m_frames.size() + off];
    }
    void pushFrame(IFunction *func, int localCount){ m_frames.push_back(StackFrame(func, localCount)); } 
    void popFrame() { m_frames.pop_back(); }

    // TODO: opmitize
    StackFrame* getFrameByLevel(int level);
private:
    Runtime(Runtime& o);
    Runtime& operator = (Runtime& o);
    Runtime();
    ~Runtime();
private:
    LuaTable *m_gtable;
    vector<StackFrame> m_frames;
};

#endif
