
#include "pch.h"
#include "JSFunction.h"
#include "JSVM.h"
#include "ByteCode.h"

JSValue Function::callFromC(JSValue* argsBegin, JSValue* argsEnd) {
    JSValue r;
    auto vm = JSVM::instance();
    auto frame = vm->topFrame();
    JSValue *ret = vm->pushStack(argsBegin, argsEnd);
    callFromVM(ret, ret + (argsEnd - argsBegin));
    execute(frame);
    r = *ret;
    vm->popStack(int(argsEnd - argsBegin));
    return r;
}
void Function::callFromVM(JSValue *argsBegin, JSValue* argsEnd) {
    if (funcType == FT_C) {
        auto cfunc = static_cast<CFunction*>(this);
        if (cfunc->func(argsBegin, argsEnd) == 0) {
            *argsBegin = JSValue::NIL;
        }
    } else {
        ASSERT(funcType == FT_JS);
        auto jsFunc = static_cast<JSFunction*>(this);
        int paramCount = (int)(argsEnd - argsBegin);
        for (; paramCount < jsFunc->meta->argCount; ++paramCount) {
            argsBegin[paramCount] = JSValue::NIL;
        }
        JSVM::instance()->pushFrame(jsFunc, argsBegin);
    }
}
