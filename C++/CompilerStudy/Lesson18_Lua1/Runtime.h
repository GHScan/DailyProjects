
#ifndef RUNTIME_H
#define RUNTIME_H

#include "LuaValue.h"

class LuaTable;
class CFunction;
class LuaFunction;

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
        return m_frames[m_frames.size() + off];
    }
    void pushFrame(LuaFunction *func, int localCount);
    void pushFrame(CFunction *func);
    void popFrame();

    StackFrame* getFrameByLevel(int level);
private:
    Runtime(Runtime& o);
    Runtime& operator = (Runtime& o);
    Runtime();
    ~Runtime();
private:
    LuaTable *m_gtable;
    vector<StackFrame*> m_frames;
    vector<vector<StackFrame*> > m_levelFrames;
};

#endif
