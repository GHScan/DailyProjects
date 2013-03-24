
#ifndef LUA_FUNCTION_H
#define LUA_FUNCTION_H

#include "Function.h"

struct LuaFunctionMeta {
    string name;
};

class LuaFunction:
    public IFunction {
public:
    LuaFunction(LuaFunctionMeta *meta);
    ~LuaFunction();

    const LuaFunctionMeta* getMeta() const { return m_meta; }

    virtual const string& getName() { return m_meta->name; }
    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets);
private:
    LuaFunction(LuaFunction& o);
    LuaFunction& operator = (const LuaFunction& o);

private:
    LuaFunctionMeta *m_meta;
};

#endif
