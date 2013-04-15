
#ifndef LUA_FUNCTION_H
#define LUA_FUNCTION_H

#include "LuaValue.h"
#include "GCObject.h"

struct IStmtNode;
typedef shared_ptr<IStmtNode> StmtNodePtr;

struct Function: 
    public GCObject {
    enum FuncType {
        FT_Lua,
        FT_C,
    };
    FuncType funcType;
    Function(FuncType _funcType);
    bool equal(Function *o);
    void collectGCObject(vector<GCObject*>& unscaned);
    void destroy();
};

struct LuaFunctionMeta {
    int argCount, localCount;
    int level;
    vector<int> codes;
    StmtNodePtr ast;
    vector<LuaValue> constTable;
    vector<pair<int, int> > upValues;

    LuaFunctionMeta(): argCount(0), localCount(0), level(0){}
    int getConstIdx(const LuaValue& v);
};

struct LuaFunction:
    public Function {

    static LuaFunction* create(LuaFunctionMeta *meta) {
        return new LuaFunction(meta);
    }

    LuaFunctionMeta *meta;
    vector<LuaValue> upValues;
private:
    LuaFunction(LuaFunctionMeta *_meta): Function(FT_Lua), meta(_meta){}
};

typedef void (*CFuncT)(const vector<LuaValue>& args, vector<LuaValue>& rets);
struct CFunction:
    public Function {

    static CFunction* create(CFuncT func) {
        return new CFunction(func);
    }

    CFuncT func;
private:
    CFunction(CFuncT _func): Function(FT_C), func(func){}
};

struct CFuncEntry {
    const char *name;
    CFuncT func;
};

void callFunc(int tempIdx);
void callFunc(LuaValue &func, const vector<LuaValue>& params, vector<LuaValue>& rets);

inline bool Function::equal(Function *o) {
    if (funcType == o->funcType) {
        if (funcType == FT_Lua) {
            auto lfunc1 = static_cast<LuaFunction*>(this), lfunc2 = static_cast<LuaFunction*>(o);
            return lfunc1->meta == lfunc2->meta && lfunc1->upValues == lfunc2->upValues;
        } else if (funcType == FT_C) {
            auto cfunc = static_cast<CFunction*>(this), cfunc2 = static_cast<CFunction*>(o);
            return cfunc->func == cfunc2->func;
        } else {
            ASSERT(0);
        }
    }
    return false;
}

#endif
