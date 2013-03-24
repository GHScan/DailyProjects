
#ifndef RUNTIME_H
#define RUNTIME_H

class LuaTable;

class Runtime {
public:
    Runtime();
    ~Runtime();

    LuaTable* getGlobalTable();
private:
    Runtime(Runtime& o);
    Runtime& operator = (Runtime& o);
private:
    LuaTable *m_gtable;
};

#endif
