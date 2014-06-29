#ifndef SINTERPRETER_H
#define SINTERPRETER_H

#include "SValue.h"

struct StackFrame;
struct SFuncProto;
struct SClassProto;

struct SInterpreter {

    static void call(
            int actualCount,
            vector<SValue> &evalStack,
            vector<StackFrame> &frameStack,
            SObjectManager *objMgr,
            vector<SValue> &constants,
            vector<SValue> &globals,
            vector<SFuncProto*> &fprotos,
            vector<SClassProto*> &cprotos);
};

#endif
