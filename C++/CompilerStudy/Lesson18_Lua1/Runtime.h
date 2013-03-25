
#ifndef RUNTIME_H
#define RUNTIME_H

class LuaTable;

class Runtime {
public:
    static Runtime* instance() {
        static Runtime s_ins;
        return &s_ins;
    }
    LuaTable* getGlobalTable() { return m_gtable; }
private:
    Runtime(Runtime& o);
    Runtime& operator = (Runtime& o);
    Runtime();
    ~Runtime();
private:
    LuaTable *m_gtable;
};

#endif
