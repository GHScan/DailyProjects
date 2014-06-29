#include "pch.h"
#include "SInterpreter.h"
#include "SVM.h"
#include "STypes.h"
#include "SObjectManager.h"
#include "ByteCodeDefinition.h"
#include "SProto.h"

static void setupFrame(
        int actualCount,
        vector<SValue> &evalStack,
        vector<StackFrame> &frameStack,
        SObjectManager *objMgr) {

    frameStack.push_back(StackFrame{nullptr, nullptr, 0});
    auto &frame = frameStack.back();

    auto pfunc = evalStack.begin() + (evalStack.size() - actualCount - 1);

    frame.func = pfunc->getObject<SFunc>();
    {
        SValue v;
        objMgr->createObject<SEnv>(&v, frame.func->env, actualCount);
        frame.localEnv = v.getObject<SEnv>();
    }

    {
        auto iter = pfunc;
        for (int i = 0; i < actualCount; ++i) {
            frame.localEnv->setValue(i, *++iter);
        }
    }

    evalStack.erase(pfunc, evalStack.end());
}

void SInterpreter::call(
        int actualCount,
        vector<SValue> &evalStack,
        vector<StackFrame> &frameStack,
        SObjectManager *objMgr,
        vector<SValue> &constants,
        vector<SValue> &globals,
        vector<SFuncProto*> &fprotos,
        vector<SClassProto*> &cprotos) {

    setupFrame(actualCount, evalStack, frameStack, objMgr);
}
