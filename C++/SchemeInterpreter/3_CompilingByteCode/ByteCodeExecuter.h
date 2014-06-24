#ifndef BYTECODEEXECUTER_H
#define BYTECODEEXECUTER_H

#include "SValue.h"
#include "SScriptFunctionProto.h"

class SEvalStack;
class SFrameStack;
class SObjectManager;

void executeByteCode(
        int actualCount,
        SEvalStack *estack, 
        SFrameStack *fstack,
        SObjectManager *objMgr,
        vector<SValue> *globals,
        vector<SValue> *literals,
        vector<SScriptFunctionProtoPtr> *protos);

#endif
