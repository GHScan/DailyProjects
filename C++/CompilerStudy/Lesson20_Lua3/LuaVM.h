
#ifndef LUA_VM_H
#define LUA_VM_H

class GCObjectManager;
class StringPool;
struct LuaStack;
class LuaTable;
struct LuaFunctionMeta;
typedef shared_ptr<LuaFunctionMeta> LuaFunctionMetaPtr;

class LuaVM {
public:
    static void create();
    static void destroy();
    static LuaVM* instance() { return s_ins;}

    GCObjectManager* getGCObjManager() { return m_gcObjMgr; }
    StringPool *getStringPool() { return m_strPool; }
    LuaStack* getCurrentStack() { return m_curStack;}
    void setCurrentStack(LuaStack *stack) { m_curStack = stack; }
    LuaTable* getGlobalTable() { return m_gtable;}
    void setGlobalTable(LuaTable *t) { m_gtable = t; }

    int getFunctionMetaIdx(const LuaFunctionMetaPtr &meta);
    const LuaFunctionMetaPtr& getMeta(int idx) {
        return m_metas[idx];
    }
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
    LuaTable *m_gtable;
    vector<LuaFunctionMetaPtr> m_metas;
};

inline int LuaVM::getFunctionMetaIdx(const LuaFunctionMetaPtr &meta) {
    for (int i = 0; i < (int)m_metas.size(); ++i) {
        if (meta == m_metas[i]) return i;
    }
    m_metas.push_back(meta);
    return (int)m_metas.size() - 1;
}

#endif

