
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
        upValues.push_back(&upFrame->local(uvInfo.second));
    }
}

int LuaFunctionMeta::getConstIdx(const LuaValue& v) {
    for (int i = 0; i < (int)constTable.size(); ++i) {
        if (constTable[i] == v) return i;
    }
    constTable.push_back(v);
    return (int)constTable.size() - 1;
}

void callFunc(int tempIdx) {
    auto stack = LuaVM::instance()->getCurrentStack();
    auto frame = stack->topFrame();
    auto func = frame->temp(tempIdx).getFunction();
    if (func->funcType == Function::FT_Lua) {
        {
            auto meta = static_cast<LuaFunction*>(func)->meta;
            int paramCount = frame->tempExtCount - tempIdx - 1;
            for (; paramCount < meta->argCount; ++paramCount) frame->pushExtTemp(LuaValue::NIL);
        }
        stack->pushFrame(func, frame->tempBase + tempIdx + 1, frame->tempExtCount - tempIdx - 1);
    } else if (func->funcType == Function::FT_C) {
        // TODO: optmize
        vector<LuaValue> params(&frame->temp(0) + tempIdx + 1, &frame->temp(0) + frame->tempExtCount);
        while (!params.empty() && params.back().isNil()) params.pop_back();
        vector<LuaValue> rets;
        stack->pushFrame(func, frame->tempBase + tempIdx + 1, frame->tempExtCount - tempIdx - 1);
        static_cast<CFunction*>(func)->func(params, rets);
        stack->popFrame();
        frame->popTemps(tempIdx);
        if (rets.empty()) frame->pushTemp(LuaValue::NIL);
        else {
            frame->pushTemp(rets[0]);
            for (int i = 1; i < (int)rets.size(); ++i) frame->pushExtTemp(rets[i]);
        }
    } else {
        ASSERT(0);
    }
}

void callFunc(const LuaValue &func, const vector<LuaValue>& args, vector<LuaValue>& rets) {
    auto stack = LuaVM::instance()->getCurrentStack();
    auto frame = stack->topFrame();
    int tempIdx = frame->tempCount;
    frame->pushTemp(func);
    for (auto &v : args) frame->pushTemp(v);
    callFunc(tempIdx);
    execute(frame);
    rets.assign(&frame->temp(0) + tempIdx, &frame->temp(0) + frame->tempExtCount);
    frame->popTemps(tempIdx);
}
