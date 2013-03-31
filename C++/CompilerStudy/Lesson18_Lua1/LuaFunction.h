
#ifndef LUA_FUNCTION_H
#define LUA_FUNCTION_H

#include "Function.h"
#include "LuaValue.h"
#include "Ast.h"

//======== LuaFunctionMeta ============
struct LuaFunctionMeta {
    int argCount, level, localCount;
    vector<LuaValue> constTable;
    vector<string> nameTable;
    StmtNodePtr body;
    vector<pair<int, int> > upValues;

    LuaFunctionMeta(int _level): argCount(0), level(_level), localCount(0){}
    int getConstIndex(const LuaValue& v);
    int getNameIndex(const string& name);

    static std::stack<shared_ptr<LuaFunctionMeta> >* stack() {
        static std::stack<shared_ptr<LuaFunctionMeta> > s_ins;
        return &s_ins;
    }
};
typedef shared_ptr<LuaFunctionMeta> LuaFunctionMetaPtr;
//======== LuaFunction ============
class LuaFunction:
    public IFunction {
public:
    static LuaFunction* create(LuaFunctionMetaPtr meta) {
        return new LuaFunction(meta);
    }

    LuaValue& getUpValue(int idx);
    const LuaFunctionMeta* getMeta() const { return m_meta.get(); }

    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets);
    virtual bool equal(IFunction *o);

private:
    LuaFunction(LuaFunction& o);
    LuaFunction& operator = (const LuaFunction& o);
    LuaFunction(LuaFunctionMetaPtr meta);
    ~LuaFunction();

private:
    LuaFunctionMetaPtr m_meta;
    vector<LuaValue> m_upValues;
};

#endif
