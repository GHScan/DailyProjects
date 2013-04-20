
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
    int level, line;
    vector<int> codes;
    StmtNodePtr ast;
    vector<LuaValue> constTable;
    vector<pair<int, int> > upValues;

    LuaFunctionMeta(): argCount(0), localCount(0), level(0), line(0){}
    int getConstIdx(const LuaValue& v);
};
typedef shared_ptr<LuaFunctionMeta> LuaFunctionMetaPtr;

struct LuaFunction:
    public Function {

    static LuaFunction* create(const LuaFunctionMetaPtr &meta) {
        return new LuaFunction(meta);
    }

    LuaFunctionMetaPtr meta;
    vector<LuaValue> upValues;

    LuaValue& upValue(int uvIdx) {
        return upValues[uvIdx];
    }
private:
    LuaFunction(const LuaFunctionMetaPtr &_meta): Function(FT_Lua), meta(_meta){}
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
void callFunc(const LuaValue &func, const vector<LuaValue>& args, vector<LuaValue>& rets);

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
