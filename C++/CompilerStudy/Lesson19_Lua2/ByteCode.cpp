
#include "pch.h"
#include "ByteCode.h"
#include "LuaVM.h"
#include "LuaStack.h"
#include "LuaFunction.h"

void execute(LuaStackFrame *stopFrame) {
    for (;;) {
        auto frame = LuaVM::instance()->getCurrentStack()->topFrame();
        if (frame == stopFrame) break;
        auto lfunc = static_cast<LuaFunction*>(frame->func);
        int code = lfunc->meta->codes[frame->ip];
        switch (code) {
            default: ASSERT(0);
        }
    }
}
