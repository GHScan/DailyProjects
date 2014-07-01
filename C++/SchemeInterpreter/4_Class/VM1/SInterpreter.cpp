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

    frameStack.push_back(StackFrame{nullptr, nullptr, 0, 0});
    auto frame = &frameStack.back();

    frame->retOff = evalStack.size() - actualCount - 1;
    frame->func = evalStack[frame->retOff].getObject<SFunc>();
    {
        SValue v;
        frame->localEnv = objMgr->createObject<SEnv>(&v, frame->func->env, actualCount);
    }
    for (int i = 0; i < actualCount; ++i) {
        frame->localEnv->setValue(i, evalStack[frame->retOff + 1 + i]);
    }

    evalStack.erase(evalStack.begin() + frame->retOff, evalStack.end());
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

    int frameOff = (int)frameStack.size();

Label_PeekFrame:
    while ((int)frameStack.size() >= frameOff) {
        auto frame = &frameStack.back();

        int pc = frame->pc;
        uint8_t *codes = frame->func->proto->codes;
        int maxPc = frame->func->proto->codeSize;
        SEnv *localEnv = frame->localEnv;

        while (pc < maxPc) {
            switch (codes[pc]) {
                case BCE_LoadBool:
                    evalStack.push_back(reinterpret_cast<ByteCode<BCE_LoadBool>*>(&codes[pc])->value == 1 ? SValue::TRUE : SValue::FALSE);
                    pc += sizeof(ByteCode<BCE_LoadBool>);
                    break;
                case BCE_LoadInt:
                    evalStack.push_back(SValue((double)reinterpret_cast<ByteCode<BCE_LoadInt>*>(&codes[pc])->value));
                    pc += sizeof(ByteCode<BCE_LoadInt>);
                    break;
                case BCE_LoadDouble:
                    evalStack.push_back(SValue(reinterpret_cast<ByteCode<BCE_LoadDouble>*>(&codes[pc])->value));
                    pc += sizeof(ByteCode<BCE_LoadDouble>);
                    break;
                case BCE_LoadConstant:
                    evalStack.push_back(constants[reinterpret_cast<ByteCode<BCE_LoadConstant>*>(&codes[pc])->kindex]);
                    pc += sizeof(ByteCode<BCE_LoadConstant>);
                    break;
                case BCE_LoadGlobal:
                    evalStack.push_back(globals[reinterpret_cast<ByteCode<BCE_LoadGlobal>*>(&codes[pc])->gindex]);
                    pc += sizeof(ByteCode<BCE_LoadGlobal>);
                    break;
                case BCE_StoreGlobal:
                    globals[reinterpret_cast<ByteCode<BCE_StoreGlobal>*>(&codes[pc])->gindex] = evalStack.back();
                    evalStack.pop_back();
                    pc += sizeof(ByteCode<BCE_StoreGlobal>);
                    break;
                case BCE_LoadLocal:
                    evalStack.push_back(localEnv->getValue(reinterpret_cast<ByteCode<BCE_LoadLocal>*>(&codes[pc])->lindex));
                    pc += sizeof(ByteCode<BCE_LoadLocal>);
                    break;
                case BCE_StoreLocal:
                    localEnv->setValue(reinterpret_cast<ByteCode<BCE_StoreLocal>*>(&codes[pc])->lindex, evalStack.back());
                    evalStack.pop_back();
                    pc += sizeof(ByteCode<BCE_StoreLocal>);
                    break;
                case BCE_LoadFree: {
                    auto ins = reinterpret_cast<ByteCode<BCE_LoadFree>*>(&codes[pc]);
                    evalStack.push_back(localEnv->getUpEnv(ins->envIndex)->getValue(ins->index));
                    pc += sizeof(ByteCode<BCE_LoadFree>);
                   }
                    break;
                case BCE_LoadFree1:
                    evalStack.push_back(localEnv->prevEnv->getValue(reinterpret_cast<ByteCode<BCE_LoadFree1>*>(&codes[pc])->index));
                    pc += sizeof(ByteCode<BCE_LoadFree1>);
                    break;
                case BCE_LoadFree2:
                    evalStack.push_back(localEnv->prevEnv->prevEnv->getValue(reinterpret_cast<ByteCode<BCE_LoadFree2>*>(&codes[pc])->index));
                    pc += sizeof(ByteCode<BCE_LoadFree2>);
                    break;
                case BCE_LoadFree3:
                    evalStack.push_back(localEnv->prevEnv->prevEnv->prevEnv->getValue(reinterpret_cast<ByteCode<BCE_LoadFree3>*>(&codes[pc])->index));
                    pc += sizeof(ByteCode<BCE_LoadFree3>);
                    break;
                case BCE_StoreFree: {
                    auto ins = reinterpret_cast<ByteCode<BCE_StoreFree>*>(&codes[pc]);
                    localEnv->getUpEnv(ins->envIndex)->setValue(ins->index, evalStack.back());
                    evalStack.pop_back();
                    pc += sizeof(ByteCode<BCE_StoreFree>);
                }
                    break;
                case BCE_StoreFree1: {
                    localEnv->prevEnv->setValue(reinterpret_cast<ByteCode<BCE_StoreFree1>*>(&codes[pc])->index, evalStack.back());
                    evalStack.pop_back();
                    pc += sizeof(ByteCode<BCE_StoreFree1>);
                 }
                    break;
                case BCE_StoreFree2: {
                    localEnv->prevEnv->prevEnv->setValue(reinterpret_cast<ByteCode<BCE_StoreFree2>*>(&codes[pc])->index, evalStack.back());
                    evalStack.pop_back();
                    pc += sizeof(ByteCode<BCE_StoreFree2>);
                 }
                    break;
                case BCE_StoreFree3: {
                    localEnv->prevEnv->prevEnv->prevEnv->setValue(reinterpret_cast<ByteCode<BCE_StoreFree3>*>(&codes[pc])->index, evalStack.back());
                    evalStack.pop_back();
                    pc += sizeof(ByteCode<BCE_StoreFree3>);
                 }
                    break;
                case BCE_LoadFunc:
                    evalStack.push_back(SValue::EMPTY);
                    objMgr->createObject<SFunc>(&evalStack.back(), localEnv, fprotos[reinterpret_cast<ByteCode<BCE_LoadFunc>*>(&codes[pc])->findex]);
                    pc += sizeof(ByteCode<BCE_LoadFunc>);
                    break;
                case BCE_Pop:
                    evalStack.pop_back();
                    pc += sizeof(ByteCode<BCE_Pop>);
                    break;
                case BCE_Jmp:
                    pc = reinterpret_cast<ByteCode<BCE_Jmp>*>(&codes[pc])->target;
                    break;
                case BCE_TrueJmp: {
                    bool b = evalStack.back() == SValue::TRUE;
                    evalStack.pop_back();
                    if (b) {
                        pc = reinterpret_cast<ByteCode<BCE_TrueJmp>*>(&codes[pc])->target;
                    } else {
                        pc += sizeof(ByteCode<BCE_TrueJmp>);
                    }
                  }
                    break;
                case BCE_Tail:
                    frameStack.pop_back();
                    frame = nullptr;
                    pc += sizeof(ByteCode<BCE_Tail>);
                    break;
                case BCE_Call: {
                        int actualCount = reinterpret_cast<ByteCode<BCE_Call>*>(&codes[pc])->actualCount;
                        pc += sizeof(ByteCode<BCE_Call>);

                        if (frame != nullptr) {
                            frame->pc = pc;
                        }

                        auto pfunc = evalStack.begin() + (evalStack.size() - actualCount - 1);
                        switch (pfunc->getType()) {
                            case SVT_Func: {
                                setupFrame(actualCount, evalStack, frameStack, objMgr);

                                goto Label_PeekFrame;
                               }
                                break;
                            case SVT_NativeFunc: {
                                pfunc->getObject<SNativeFunc>()->func(objMgr, &*pfunc);
                                evalStack.erase(++pfunc, evalStack.end());
                             }
                                break;
                            case SVT_Class: {
                                auto sclass = pfunc->getObject<SClass>();
                                ASSERT(sclass->proto->fieldCount == actualCount);

                                auto obj = objMgr->createObject<SEnv>(&*pfunc, sclass->asEnv(), actualCount);
                                {
                                    auto iter = pfunc;
                                    for (int i = 0; i < actualCount; ++i) {
                                        obj->setValue(i, *++iter);
                                    }
                                }

                                evalStack.erase(++pfunc, evalStack.end());
                            }
                                break;
                            default:
                                ASSERT(0);
                                break;
                        }

                        if (frame == nullptr) {
                            goto Label_PeekFrame;
                        } 
                   }
                    break;
                case BCE_LoadClass:
                    evalStack.push_back(SValue::EMPTY);
                    objMgr->createObject<SClass>(&evalStack.back(), localEnv, cprotos[reinterpret_cast<ByteCode<BCE_LoadClass>*>(&codes[pc])->cindex]);
                    pc += sizeof(ByteCode<BCE_LoadClass>);
                    break;
                case BCE_LoadMethod: {
                    auto ins = reinterpret_cast<ByteCode<BCE_LoadMethod>*>(&codes[pc]);
                    auto sclass = reinterpret_cast<SClass*>(localEnv->getUpEnv(ins->envIndex));
                    auto f = sclass->proto->methodInfos[ins->index].funcProto;
                    reinterpret_cast<ByteCode<BCE_LoadCachedMethod>*>(ins)->updateCache(ins->envIndex - 1, f);
                 }
                    break;
                case BCE_LoadCachedMethod: {
                    auto ins = reinterpret_cast<ByteCode<BCE_LoadCachedMethod>*>(&codes[pc]);
                    evalStack.push_back(SValue::EMPTY);
                    objMgr->createObject<SFunc>(&evalStack.back(), localEnv->getUpEnv(ins->cachedObjIndex), ins->cachedFunc);
                    pc += sizeof(ByteCode<BCE_LoadCachedMethod>);
                   }
                    break;
                case BCE_GetField: {
                    auto ins = reinterpret_cast<ByteCode<BCE_GetField>*>(&codes[pc]);
                    auto cproto = reinterpret_cast<SClass*>(evalStack.back().getObject<SEnv>()->prevEnv)->proto;

                    auto name = constants[ins->kindex].getSymbol();
                    int index = 0;
                    for (; index < cproto->fieldCount; ++index) {
                        if (cproto->fieldInfos[index].name == name) break;
                    }
                    ASSERT(index < cproto->fieldCount);

                    reinterpret_cast<ByteCode<BCE_GetCachedField>*>(ins)->updateCache(cproto, index);
                   }
                    break;
                case BCE_GetCachedField: {
                    auto ins = reinterpret_cast<ByteCode<BCE_GetCachedField>*>(&codes[pc]);
                    auto obj = evalStack.back().getObject<SEnv>();
                    if (reinterpret_cast<SClass*>(obj->prevEnv)->proto == ins->cachedClass) {
                        evalStack.back() = obj->getValue(ins->cachedIndex);
                        pc += sizeof(ByteCode<BCE_GetCachedField>);
                    } else {
                        ins->discardCache();
                    }
                 }
                    break;
                case BCE_GetMethod: {
                    auto ins = reinterpret_cast<ByteCode<BCE_GetMethod>*>(&codes[pc]);
                    auto cproto = reinterpret_cast<SClass*>(evalStack.back().getObject<SEnv>()->prevEnv)->proto;

                    auto name = constants[ins->kindex].getSymbol();
                    SFuncProto *fproto = nullptr;
                    for (int i = 0; i < cproto->methodCount; ++i) {
                        if (cproto->methodInfos[i].name == name) {
                            fproto = cproto->methodInfos[i].funcProto;
                            break;
                        }
                    }
                    ASSERT(fproto != nullptr);

                    reinterpret_cast<ByteCode<BCE_GetCachedMethod>*>(ins)->updateCache(cproto, fproto);
                }
                    break;
                case BCE_GetCachedMethod: {
                    auto ins = reinterpret_cast<ByteCode<BCE_GetCachedMethod>*>(&codes[pc]);
                    auto obj = evalStack.back().getObject<SEnv>();
                    if (reinterpret_cast<SClass*>(obj->prevEnv)->proto == ins->cachedClass) {
                        objMgr->createObject<SFunc>(&evalStack.back(), obj, ins->cachedFunc);
                        pc += sizeof(ByteCode<BCE_GetCachedMethod>);
                    } else {
                        ins->discardCache();
                    }
                  }
                    break;
                default:
                    ASSERT(0);
                    break;
            }
        }

        frameStack.pop_back();
    }

}
