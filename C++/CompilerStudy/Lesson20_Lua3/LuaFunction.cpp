
#include "pch.h"

#include "LuaFunction.h"
#include "LuaVM.h"
#include "LuaStack.h"
#include "ByteCode.h"
#include "GCObject.h"
#include "LuaTable.h"

Function::Function(FuncType _funcType): GCObject(OT_Function), funcType(_funcType){
    LuaVM::instance()->getGCObjManager()->linkObject(this);
}

void Function::collectGCObject(vector<GCObject*>& unscaned) {
    if (funcType == FT_Lua) {
        auto lfunc = static_cast<LuaFunction*>(this);
        for (auto uv : lfunc->upValues) {
            if (auto p = uv->gcAccess()) unscaned.push_back(p);
        }
        for (auto &c : lfunc->meta->constTable) {
            if (auto p = c.gcAccess()) unscaned.push_back(p);
        }
        if (auto p = lfunc->fenvTable->gcAccess()) unscaned.push_back(p);
    }
}
void Function::destroy() {
    if (funcType == FT_Lua) {
        delete static_cast<LuaFunction*>(this);
    } else if (funcType == FT_C) {
        delete static_cast<CFunction*>(this);
    } else {
        ASSERT(0);
    }
}

LuaFunction::LuaFunction(const LuaFunctionMetaPtr &_meta): 
    Function(FT_Lua), meta(_meta), fenvTable(LuaVM::instance()->getGlobalTable()) {
    auto stack = LuaVM::instance()->getCurrentStack();
    for (int i = 0; i < (int)meta->upValues.size(); ++i) {
        auto& uvInfo = meta->upValues[i];
        auto upFrame = stack->topFrameOfLevel(uvInfo.first);
        upFrame->closures.insert(make_pair(uvInfo.second, make_pair(this, i)));
        upValues.push_back(&upFrame->localPtr[uvInfo.second]);
    }
}

int LuaFunctionMeta::getConstIdx(const LuaValue& v) {
    for (int i = 0; i < (int)constTable.size(); ++i) {
        if (constTable[i] == v) return i;
    }
    constTable.push_back(v);
    return (int)constTable.size() - 1;
}

void callFunc(int funcIdx, int paramCount, int requireRetN) {
    auto stack = LuaVM::instance()->getCurrentStack();
    auto func = stack->values()[funcIdx].getFunction();
    if (func->funcType == Function::FT_Lua) {
        auto stack = LuaVM::instance()->getCurrentStack();
        auto lfunc = static_cast<LuaFunction*>(func);
        int paramBase = funcIdx + 1;
        {
            int reqParamCount = lfunc->meta->argCount;
            if (paramCount < reqParamCount) {
                stack->reserveValueSpace(paramBase + reqParamCount);
                for (int i = paramCount; i < reqParamCount; ++i) {
                    stack->values()[paramBase + i] = LuaValue::NIL;
                }
                paramCount = reqParamCount;
            }
        }
        stack->pushFrame(lfunc, paramBase, paramCount, requireRetN);
    } else if (func->funcType == Function::FT_C) {
        auto stack = LuaVM::instance()->getCurrentStack();
        auto &values = stack->values();
        vector<LuaValue> args(values.begin() + funcIdx + 1, values.begin() + funcIdx + 1 + paramCount); 
        vector<LuaValue> rets;
        static_cast<CFunction*>(func)->func(args, rets);
        if (requireRetN > 0) rets.resize(requireRetN, LuaValue::NIL);
        if (rets.empty()) rets.push_back(LuaValue::NIL);
        stack->reserveValueSpace(funcIdx + (int)rets.size());
        std::copy(rets.begin(), rets.end(), values.begin() + funcIdx);
        stack->topFrame()->setExtCount((int)rets.size());
    } else {
        ASSERT(0);
    }
}

void callFunc(const LuaValue& func, const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto stack = LuaVM::instance()->getCurrentStack();
    auto &values = stack->values();
    int funcIdx = (int)values.size();
    values.push_back(func);
    values.insert(values.end(), args.begin(), args.end());
    auto frame = stack->topFrame();
    callFunc(funcIdx, (int)args.size(), 0);
    execute(frame);
    rets.assign(values.begin() + funcIdx, values.begin() + funcIdx + frame->getExtCount());
    values.resize(funcIdx);
}

