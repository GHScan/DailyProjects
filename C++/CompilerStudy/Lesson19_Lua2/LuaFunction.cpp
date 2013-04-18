
#include "pch.h"

#include "LuaFunction.h"
#include "LuaVM.h"
#include "LuaStack.h"
#include "ByteCode.h"
#include "GCObject.h"

Function::Function(FuncType _funcType): GCObject(OT_Function), funcType(_funcType){
    LuaVM::instance()->getGCObjManager()->linkObject(this);
}

void Function::collectGCObject(vector<GCObject*>& unscaned) {
    if (funcType == FT_Lua) {
        for (auto &uv : static_cast<LuaFunction*>(this)->upValues) {
            if (auto p = uv.gcAccess()) unscaned.push_back(p);
        }
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
        stack->pushFrame(func, tempIdx + 1, frame->tempExtCount - tempIdx - 1);
    } else if (func->funcType == Function::FT_C) {
        // TODO: optmize
        vector<LuaValue> params(&frame->temp(tempIdx + 1), &frame->temp(frame->tempExtCount));
        vector<LuaValue> rets;
        stack->pushFrame(func, tempIdx + 1, frame->tempExtCount - tempIdx - 1);
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
    rets.assign(&frame->temp(tempIdx), &frame->temp(frame->tempExtCount));
    frame->popTemps(tempIdx);
}
