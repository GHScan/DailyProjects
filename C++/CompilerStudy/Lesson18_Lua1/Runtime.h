
#ifndef RUNTIME_H
#define RUNTIME_H

class LuaTable;
struct IFunction;

class Runtime {
public:
    static Runtime* instance() {
        static Runtime s_ins;
        return &s_ins;
    }

    LuaTable* getGlobalTable() { return m_gtable; } 
    void setGlobalTable(LuaTable *t);

    IFunction* getFrame(int off) { 
        ASSERT(off < 0);
        return m_frames[m_frames.size() + off];
    }
    void pushFrame(IFunction *func){ m_frames.push_back(func); } 
    void popFrame() { m_frames.pop_back(); }
private:
    Runtime(Runtime& o);
    Runtime& operator = (Runtime& o);
    Runtime();
    ~Runtime();
private:
    LuaTable *m_gtable;
    vector<IFunction*> m_frames;
};

#endif
