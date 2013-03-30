
#ifndef FUNCTION_H
#define FUNCTION_H

#include "LuaValue.h"

struct IFunction {
public:
    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets) = 0;
    virtual bool equal(IFunction *o) = 0;

    LuaTable* getfenv() { return m_fenv; }
    void setfenv(LuaTable* env);

    int getRefCount() const { return m_refCount;}
    int addRef() { return ++m_refCount;}
    int releaseRef() {
        int r = --m_refCount;
        if (r == 0) delete this;
        return r;
    }

protected:
    IFunction();
    virtual ~IFunction();
    IFunction(IFunction& o);
    IFunction& operator = (const IFunction& o);

private:
    int m_refCount;
    LuaTable* m_fenv;
};
typedef shared_ptr<IFunction> FunctionPtr;

class CFunction:
    public IFunction {
public:
    typedef void (*CFuncT)(const vector<LuaValue>& args, vector<LuaValue>& rets);
public:
    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets);
    virtual bool equal(IFunction *o);
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

struct CFuncEntry
{
    string name;
    void (*func)(const vector<LuaValue>& args, vector<LuaValue>& rets);
};

#endif
