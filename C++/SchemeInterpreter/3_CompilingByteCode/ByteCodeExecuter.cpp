#include "pch.h"
#include "ByteCodeExecuter.h"
#include "SStack.h"
#include "SObjectManager.h"
#include "SScriptFunctionProto.h"
#include "ByteCodeDefinition.h"

static void setupFrame(
        int actualCount,
        SEvalStack *estack,
        SFrameStack *fstack,
        SObjectManager *objMgr) {

    auto frame = fstack->allocFrame();
    frame->func = estack->top(-actualCount - 1).getObject()->staticCast<SScriptFunction>();
    auto proto = frame->func->proto;
    {
        SValue v;
        frame->localEnv = objMgr->createEnv(&v, frame->func->env, (int)proto->locals.size());
    }

    ASSERT(actualCount == proto->formalCount && "Argument count mistmatch");
    for (int i = 0; i < actualCount; ++i) {
        frame->localEnv->setLocal(i, estack->top(-actualCount + i));
    }

    estack->pop(actualCount + 1);
}

void executeByteCode(
        int actualCount,
        SEvalStack *estack,
        SFrameStack *fstack,
        SObjectManager *objMgr,
        vector<SValue> *globals,
        vector<SValue> *literals,
        vector<SScriptFunctionProtoPtr> *protos) {

    int frameOff = fstack->size();

    setupFrame(actualCount, estack, fstack, objMgr);

Label_PeekFrame:
    while (fstack->size() > frameOff) {
        // every pointer here will not be redirect after compaction GC
        auto frame = fstack->top();
        int pc = frame->pc;
        int maxPc = (int)frame->func->proto->bytes.size();
        uint8_t *bytes = &frame->func->proto->bytes[0];
        SValue *pGlobals = &(*globals)[0];
        SValue *pLiterals = &(*literals)[0];
        SScriptFunctionProtoPtr *pProtos = &(*protos)[0];

        while (pc < maxPc) {
            switch (bytes[pc]) {
                case ByteCode_LoadLiteral::CODE:
                    estack->push(pLiterals[static_cast<ByteCode_LoadLiteral*>((void*)&bytes[pc])->literalIndex]);
                    pc += sizeof(ByteCode_LoadLiteral);
                    break;
                case ByteCode_LoadLocal::CODE:
                    estack->push(frame->localEnv->getLocal(static_cast<ByteCode_LoadLocal*>((void*)&bytes[pc])->localIndex));
                    pc += sizeof(ByteCode_LoadLocal);
                    break;
                case ByteCode_StoreLocal::CODE:
                    frame->localEnv->setLocal(static_cast<ByteCode_StoreLocal*>((void*)&bytes[pc])->localIndex, estack->pop());
                    pc += sizeof(ByteCode_StoreLocal);
                    break;
                case ByteCode_LoadGlobal::CODE:
                    estack->push(pGlobals[static_cast<ByteCode_LoadGlobal*>((void*)&bytes[pc])->globalIndex]);
                    pc += sizeof(ByteCode_LoadGlobal);
                    break;
                case ByteCode_StoreGlobal::CODE:
                    pGlobals[static_cast<ByteCode_StoreGlobal*>((void*)&bytes[pc])->globalIndex] = estack->pop();
                    pc += sizeof(ByteCode_StoreGlobal);
                    break;
                case ByteCode_LoadFree::CODE:
                    estack->push(frame->func->getFree(static_cast<ByteCode_LoadFree*>((void*)&bytes[pc])->freeIndex));
                    pc += sizeof(ByteCode_LoadFree);
                    break;
                case ByteCode_StoreFree::CODE:
                    frame->func->setFree(static_cast<ByteCode_StoreFree*>((void*)&bytes[pc])->freeIndex, estack->pop());
                    pc += sizeof(ByteCode_StoreFree);
                    break;
                case ByteCode_LoadLambda::CODE:
                    objMgr->createScriptFunction(estack->alloc(), pProtos[static_cast<ByteCode_LoadLambda*>((void*)&bytes[pc])->protoIndex].get(), frame->localEnv);
                    pc += sizeof(ByteCode_LoadLambda);
                    break;
                case ByteCode_Jmp::CODE:
                    pc = static_cast<ByteCode_Jmp*>((void*)&bytes[pc])->target;
                    break;
                case ByteCode_TrueJmp::CODE:
                    if (estack->pop() == SValue::TRUE) {
                        pc = static_cast<ByteCode_TrueJmp*>((void*)&bytes[pc])->target;
                    } else {
                        pc += sizeof(ByteCode_TrueJmp);
                    }
                    break;
                case ByteCode_Tail::CODE:
                    fstack->pop();
                    frame = nullptr;
                    pc += sizeof(ByteCode_Tail);
                    break;
                case ByteCode_Call::CODE: {
                    int actualCount = static_cast<ByteCode_Call*>((void*)&bytes[pc])->actualCount;
                    pc += sizeof(ByteCode_Call);

                    SValue f = estack->top(-actualCount - 1);
                    if (f.getType() == SCFunction::TYPE) {
                        f.getExternalObject()->staticCast<SCFunction>()->func(objMgr, estack, actualCount);
                        estack->pop(actualCount);

                    } else {
                        if (frame != nullptr) {
                            frame->pc = pc;
                        }

                        setupFrame(actualCount, estack, fstack, objMgr);

                        goto Label_PeekFrame;
                    }
                  }
                    break;
                case ByteCode_Pop::CODE:
                    estack->pop();
                    pc += sizeof(ByteCode_Pop);
                    break;
                default:
                    ASSERT(0);
                    break;
            }
        }

        fstack->pop();
    }
}
