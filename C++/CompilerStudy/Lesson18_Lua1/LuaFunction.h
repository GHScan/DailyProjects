
#ifndef LUA_FUNCTION_H
#define LUA_FUNCTION_H

#include "Function.h"
#include "LuaValue.h"
#include "Ast.h"

//======== LuaFunctionMeta ============
struct LuaFunctionMeta {
    int argCount;
    vector<LuaValue> constTable;
    vector<string> nameTable;
    StmtNodePtr body;

    LuaFunctionMeta(): argCount(0){}
    int getConstIndex(const LuaValue& v);
    int getNameIndex(const string& name);
};
typedef shared_ptr<LuaFunctionMeta> LuaFunctionMetaPtr;
//======== LuaFunction ============
class LuaFunction:
    public IFunction {
public:
    static LuaFunction* create(LuaFunctionMetaPtr meta, const vector<LuaValue>& upValues) {
        return new LuaFunction(meta, upValues);
    }

    LuaValue& getUpValue(int idx);
    const LuaFunctionMeta* getMeta() const { return m_meta.get(); }

    virtual void call(const vector<LuaValue>& args, vector<LuaValue>& rets);
    virtual bool equal(IFunction *o);

private:
    LuaFunction(LuaFunction& o);
    LuaFunction& operator = (const LuaFunction& o);
    LuaFunction(LuaFunctionMetaPtr meta, const vector<LuaValue>& upValues);
    ~LuaFunction();

private:
    LuaFunctionMetaPtr m_meta;
    vector<LuaValue> m_upValues;
};

#endif
