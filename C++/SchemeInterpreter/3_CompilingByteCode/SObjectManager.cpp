#include "pch.h"
#include "SObjectManager.h"

SObjectManager::SObjectManager(int initObjHeapSize, int initExternalObjThreshold): 
    mExternalObjCount(0), mExternalObjThreshold(initExternalObjThreshold), mFirstExternalObj(nullptr), 
    mWorkingHeap(initObjHeapSize, 0), mFreeHeap(initObjHeapSize, 0), mFreeOfWorkingHeap(0), mFreeOfFreeHeap(0),
    mSymbolMgr(nullptr) {
    mSymbolMgr = new SSymbolManager();
}

SObjectManager::~SObjectManager() {
    DELETE(mSymbolMgr);

    performFullGC();
}

SObject* SObjectManager::getForwardedObject(SObject *obj) {
    if (obj == nullptr) return nullptr;
    if (obj->isForwarded()) {
        return static_cast<SObject*>(obj->getForwardedPtr());
    }

    int bytes = obj->getAlignedSize() * SObject::ALIGNMENT;

    ASSERT((char*)obj >= &mWorkingHeap[0] && (char*)obj + bytes <= &mWorkingHeap[0] + mWorkingHeap.size());

    void *newAddress = &mFreeHeap[mFreeOfFreeHeap];
    memcpy(newAddress, obj, bytes);
    obj->forward(newAddress);

    mFreeOfFreeHeap += bytes;

    return static_cast<SObject*>(newAddress);
}

void SObjectManager::mark(SExternalObject *obj) {
    if (obj && obj->mark()) {
        mMarkedExternalObjs.push(obj);
    }
}

void SObjectManager::mark(SValue *v) {
    switch (v->getType()) {
        case SVT_Undefined:
        case SVT_Reserved:
        case SVT_Bool: 
        case SVT_Int:
        case SVT_Symbol:
            break;

        case SDouble::TYPE:
        case SString::TYPE:
        case SPair::TYPE:
        case SEnv::TYPE:
        case SScriptFunction::TYPE:
            v->setObject(getForwardedObject(v->getObject()));
            break;

        case SCFunction::TYPE:
        case SBigInt::TYPE:
            mark(v->getExternalObject());
            break;

        default:
            ASSERT(0);
            break;
    }
}

void SObjectManager::performFullGC() {
    mFreeHeap.resize(max((int)mFreeHeap.size(), mFreeOfWorkingHeap * 2));
    ASSERT(mFreeOfFreeHeap == 0);

    for (auto p = ScopedValue<SValue>::getFirst(); p != nullptr; p = p->getNext()) mark(&p->value);
    for (auto p = ScopedValue<SObject*>::getFirst(); p != nullptr; p = p->getNext()) p->value = getForwardedObject(p->value);
    for (auto p = ScopedValue<SExternalObject*>::getFirst(); p != nullptr; p = p->getNext()) mark(p->value);

    if (mGCRootCollector != nullptr) {
        mGCRootCollector(this);
    }

    int scannedFree = 0;
    while (scannedFree < mFreeOfFreeHeap || !mMarkedExternalObjs.empty()) {

        while (!mMarkedExternalObjs.empty()) {
            SExternalObject *obj = mMarkedExternalObjs.front();
            mMarkedExternalObjs.pop();

            // mark obj's children
            switch (obj->getType()) {
                case SCFunction::TYPE:
                    break;
                case SBigInt::TYPE:
                    break;
                default:
                    ASSERT(0);
                    break;
            }
        }

        while (scannedFree < mFreeOfFreeHeap) {
            SObject *obj = static_cast<SObject*>(static_cast<void*>(&mFreeHeap[scannedFree]));
            switch (obj->getType()) {
                case SDouble::TYPE:
                    break;
                case SString::TYPE:
                    break;
                case SPair::TYPE: {
                    SPair *pair = obj->staticCast<SPair>();
                    mark(&pair->car);
                    mark(&pair->cdr);
                  }
                    break;
                case SEnv::TYPE: {
                    SEnv *env = obj->staticCast<SEnv>();
                    if (env->prevEnv != nullptr) {
                        mark(&env->prevEnv);
                    }
                    for (int i = 0; i < env->localCount; ++i) {
                        mark(&env->locals[i]);
                    }
                 }
                    break;
                case SScriptFunction::TYPE: {
                    SScriptFunction *func = obj->staticCast<SScriptFunction>();
                    if (func->env != nullptr) {
                        mark(&func->env);
                    }
                    func->freeVarsReady = false;
                    }
                    break;
                default:
                    ASSERT(0);
                    break;
            }

            scannedFree += obj->getAlignedSize() * SObject::ALIGNMENT;
        }
    }

    mExternalObjCount = 0;
    SExternalObject **p = &mFirstExternalObj;
    while (*p != nullptr) {
        SExternalObject *obj = *p;

        if (obj->isMarked()) {
            obj->unmark();
            p = &obj->next;
            ++mExternalObjCount;

        } else {
            *p = obj->next;

            switch (obj->getType()) {
                case SCFunction::TYPE:
                    delete obj->staticCast<SCFunction>();
                    break;
                case SBigInt::TYPE:
                    delete obj->staticCast<SBigInt>();
                    break;
                default:
                    ASSERT(0);
                    break;
            }
        }
    }

    swap(mWorkingHeap, mFreeHeap);
    mFreeOfWorkingHeap = mFreeOfFreeHeap;
    mFreeOfFreeHeap = 0;

    mExternalObjThreshold = max(mExternalObjThreshold, mExternalObjCount * 2);
}
