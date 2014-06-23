#include "pch.h"
#include "SObjectManager.h"

SObjectManager::SObjectManager(): 
    mExternalObjCount(0), mExternalObjThreshold(32), mFirstExternalObj(nullptr), 
    mWorkingHeap(128, 0), mFreeHeap(128, 0), mFreeOfWorkingHeap(0), mFreeOfFreeHeap(0),
    mSymbolMgr(nullptr) {
    mSymbolMgr = new SSymbolManager();
}

SObjectManager::~SObjectManager() {
    DELETE(mSymbolMgr);

    performFullGC();
}

SObject* SObjectManager::mark(SObject *obj) {
    if (obj == nullptr) return nullptr;
    if (obj->isRedirected()) {
        return static_cast<SObject*>(obj->getRedirectedPtr());
    }

    int bytes = obj->getAlignedSize() * SObject::ALIGNMENT;

    ASSERT((char*)obj >= &mWorkingHeap[0] && (char*)obj + bytes <= &mWorkingHeap[0] + mWorkingHeap.size());

    void *newAddress = &mFreeHeap[mFreeOfFreeHeap];
    memcpy(newAddress, obj, bytes);
    obj->redirect(newAddress);

    mFreeOfFreeHeap += bytes;

    return static_cast<SObject*>(newAddress);
}

void SObjectManager::mark(SExternalObject *obj) {
    if (obj && obj->mark()) {
        mMarkedExternalObjs.push(obj);
    }
}

static void updateField(SObjectManager *mgr, SValue *field) {
    switch (field->getType()) {
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
            field->setObject(mgr->mark(field->getObject()));
            break;

        case SCFunction::TYPE:
        case SBigInt::TYPE:
            mgr->mark(field->getExternalObject());
            break;

        default:
            ASSERT(0);
            break;
    }
}

void SObjectManager::performFullGC() {
    mFreeHeap.resize(max((int)mFreeHeap.size(), mFreeOfWorkingHeap * 2));
    ASSERT(mFreeOfFreeHeap == 0);

    mGCRootCollector(this);

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
                    updateField(this, &pair->car);
                    updateField(this, &pair->cdr);
                  }
                    break;
                case SEnv::TYPE: {
                    SEnv *env = obj->staticCast<SEnv>();
                    env->prevEnv = mark(env->prevEnv)->staticCast<SEnv>();
                    for (int i = 0; i < env->localCount; ++i) {
                        updateField(this, &env->locals[i]);
                    }
                 }
                    break;
                case SScriptFunction::TYPE: {
                    SScriptFunction *func = obj->staticCast<SScriptFunction>();
                    func->env = mark(func->env)->staticCast<SEnv>();
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
