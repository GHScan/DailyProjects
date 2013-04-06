
#include "pch.h"

#include "LuaFunction.h"
#include "LuaVM.h"
#include "LuaStack.h"
#include "ByteCode.h"

void callFunc(int tempIdx) {
    auto stack = LuaVM::instance()->getCurrentStack();
    auto frame = stack->topFrame();
    auto func = frame->temp(tempIdx).getFunction();
    if (func->funcType == Function::FT_Lua) {
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

void callFunc(LuaValue &func, const vector<LuaValue>& params, vector<LuaValue>& rets) {
    auto stack = LuaVM::instance()->getCurrentStack();
    auto frame = stack->topFrame();
    int tempIdx = frame->tempCount;
    frame->pushTemp(func);
    for (auto &v : params) frame->pushTemp(v);
    callFunc(tempIdx);
    execute(frame);
    for (int i = tempIdx; i < frame->tempExtCount; ++i) rets.push_back(frame->temp(i));
    frame->popTemps(tempIdx);
}
