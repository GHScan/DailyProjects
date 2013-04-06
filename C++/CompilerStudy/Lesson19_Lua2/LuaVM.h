
#ifndef LUA_VM_H
#define LUA_VM_H

class GCObjectManager;
class StringPool;
class LuaStack;

class LuaVM {
public:
    static void create();
    static void destroy();
    static LuaVM* instance() { return s_ins;}

    GCObjectManager* getGCObjManager() { return m_gcObjMgr; }
    StringPool *getStringPool() { return m_strPool; }
    LuaStack* getCurrentStack() { return m_curStack;}
    void setCurrentStack(LuaStack *stack) { m_curStack = stack; }
public:
    LuaVM();
    ~LuaVM();

private:
    LuaVM(const LuaVM&);
    LuaVM& operator = (const LuaVM&);

private:
    static LuaVM* s_ins;

private:
    GCObjectManager *m_gcObjMgr;
    StringPool *m_strPool;
    LuaStack *m_curStack;
};

#endif

