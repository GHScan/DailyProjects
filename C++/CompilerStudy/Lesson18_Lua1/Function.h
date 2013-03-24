
#ifndef FUNCTION_H
#define FUNCTION_H

#include "LuaValue.h"

struct IFunction {
    virtual ~IFunction(){}
    virtual const string& getName() = 0;
    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets) = 0;
};

class CFunction:
    public IFunction {
public:
    typedef void (*CFuncT)(const vector<LuaValue>& args, vector<LuaValue>& rets);
public:
    CFunction(const string& name, CFuncT func): m_name(name), m_func(func){}
    virtual const string& getName() { return m_name; }
    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
        m_func(args, rets);
    }
private:
    string m_name;
    CFuncT m_func;
};

#endif
