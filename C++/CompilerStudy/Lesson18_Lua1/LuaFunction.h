
#ifndef LUA_FUNCTION_H
#define LUA_FUNCTION_H

#include "Function.h"
#include "LuaValue.h"

//======== LuaFunctionMeta ============
struct LuaFunctionMeta {
    string file;
    int line;
    int argCount;
    vector<LuaValue> constTable;
    vector<string> nameTable;
    StmtNodePtr body;

    LuaFunctionMeta(const string& _file, int _line): file(_file), line(_line), argCount(0){}
    int getConstIndex(const LuaValue& v);
    int getNameIndex(const string& name);
};
//======== LuaFunction ============
class LuaFunction:
    public IFunction {
public:
    static LuaFunction* create(LuaFunctionMeta *meta, const vector<LuaValue>& upValues) {
        return new LuaFunction(meta, upValues);
    }

    LuaValue& getLocal(int idx);
    LuaValue& getUpValue(int idx);
    const vector<LuaValue>& getArgs() const { return m_args; }

    const LuaFunctionMeta* getMeta() const { return m_meta; }

    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets);

    int getRefCount() const { return m_refCount;}
    int addRef() { return ++m_refCount;}
    int releaseRef() {
        int r = --m_refCount;
        if (r == 0) delete this;
        return r;
    }
private:
    LuaFunction(LuaFunction& o);
    LuaFunction& operator = (const LuaFunction& o);
    LuaFunction(LuaFunctionMeta *meta, const vector<LuaValue>& upValues);
    ~LuaFunction();

private:
    int m_refCount;
    LuaFunctionMeta *m_meta;
    vector<LuaValue> m_locals;
    vector<LuaValue> m_upValues;
    vector<LuaValue> m_args;
};

#endif
