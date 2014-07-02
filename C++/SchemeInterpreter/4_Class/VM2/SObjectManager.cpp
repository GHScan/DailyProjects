#include "pch.h"
#include "SObjectManager.h"
#include "ScopedValue.h"

SObjectManager::SObjectManager(): mFreeOfWorkingHeap(0) {
    mWorkingHeap.resize(512 * 1024);
    mFreeHeap.resize(512 * 1024);
}

SObjectManager::~SObjectManager() {
    performFullGC();
}

void SObjectManager::mark(SValue *v) {
    if (v->isObject()) {
        *v = SValue(mark(v->getObject()));
    }
}

SObject* SObjectManager::mark(SObject *obj) {
    if (obj == nullptr) return obj;

    ASSERT((uint8_t*)obj >= &mFreeHeap[0] && (uint8_t*)obj + obj->getSize() <= &mFreeHeap[0] + mFreeHeap.size());

    if (obj->isForwarded()) {
        return obj->getForwardPtr();
    }

    auto newPtr = reinterpret_cast<SObject*>(&mWorkingHeap[mFreeOfWorkingHeap]);
    memcpy(newPtr, obj, obj->getSize());
    mFreeOfWorkingHeap += obj->getSize();

    obj->forward(newPtr);

    return newPtr;
}

static void updateFields(SObjectManager *mgr, SObject *obj) {
    switch (obj->getType()) {
        case SVT_Pair: {
            auto p = static_cast<SPair*>(obj);
            mgr->mark(&p->car);
            mgr->mark(&p->cdr);
           }
            break;
        case SVT_Env: {
            auto p = static_cast<SEnv*>(obj);
            mgr->mark(&p->prevEnv);
            for (int i = 0; i < p->vCount; ++i) {
                mgr->mark(&p->values[i]);
            }
          }
            break;
        case SVT_Func: {
            auto p = static_cast<SFunc*>(obj);
            mgr->mark(&p->env);
           }
            break;
        case SVT_NativeFunc: {
         }
            break;
        case SVT_Class: {
            auto p = static_cast<SClass*>(obj);
            mgr->mark(&p->env);
        }
            break;
        default:
            ASSERT(0);
            break;
    }
}

void SObjectManager::performFullGC() {
    swap(mWorkingHeap, mFreeHeap);

    mFreeOfWorkingHeap = 0;

    if (mRootCollector) {
        mRootCollector(this);
    }

    for (auto p = ScopedValue<SValue>::getFirst(); p != nullptr; p = p->getNext()) mark(&p->value);
    for (auto p = ScopedValue<SObject*>::getFirst(); p != nullptr; p = p->getNext()) mark(&p->value);

    int scanned = 0;
    while (scanned < mFreeOfWorkingHeap) {
        SObject *obj = reinterpret_cast<SObject*>(&mWorkingHeap[scanned]);
        updateFields(this, obj);
        scanned += obj->getSize();
    }
}
