
#ifndef FUNCTION_H
#define FUNCTION_H

#include "LuaValue.h"

struct IFunction {
public:
    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets) = 0;

    int getRefCount() const { return m_refCount;}
    int addRef() { return ++m_refCount;}
    int releaseRef() {
        int r = --m_refCount;
        if (r == 0) delete this;
        return r;
    }

protected:
    IFunction(): m_refCount(1){}
    virtual ~IFunction(){}

private:
    int m_refCount;
};
typedef shared_ptr<IFunction> FunctionPtr;

class CFunction:
    public IFunction {
public:
    typedef void (*CFuncT)(const vector<LuaValue>& args, vector<LuaValue>& rets);
public:
    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets) {
        m_func(args, rets);
    }
    static CFunction* create(CFuncT func) {
        return new CFunction(func);
    }
private:
    CFunction(const CFunction& o);
    CFunction& operator = (const CFunction& o);
    CFunction(CFuncT func): m_func(func){}
    ~CFunction(){}

private:
    CFuncT m_func;
};

#endif
