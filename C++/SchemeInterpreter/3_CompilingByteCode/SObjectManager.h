#ifndef SOBJECTMANAGER_H
#define SOBJECTMANAGER_H

#include "SValue.h"
#include "STypes.h"
#include "SSymbol.h"

class SObjectManager {
public:
    SObjectManager(int initObjHeapSize, int initExternalObjThreshold);
    ~SObjectManager();

    SObjectManager(const SObjectManager&) = delete;
    SObjectManager& operator = (const SObjectManager&) = delete;

    SSymbol* createSymbol(const char *s) {
        auto sym = mSymbolMgr->getSymbol(s);
        return sym;
    }

    SDouble* createDouble(double d) {
        return createObject<SDouble>(d);
    }

    SString* createString(const char *s) {
        return createObject<SString>(s);
    }

    SPair* createPair(const ScopedValue<SValue> &car, const ScopedValue<SValue> &cdr) {
        return createObject<SPair>(car, cdr);
    }

    SPair* createPair(const ScopedValue<SValue> &car) {
        return createObject<SPair>(car);
    }

    SEnv* createEnv(const ScopedValue<SObject*> &prevEnv, int localCount) {
        return createObject<SEnv>(prevEnv, localCount);
    }

    SScriptFunction* createScriptFunction(SScriptFunctionProto *proto, const ScopedValue<SObject*> &env) {
        return createObject<SScriptFunction>(proto, env);
    }

    SCFunction* createCFunction(CFunction f) {
        return createExternalObject<SCFunction>(f);
    }

    SBigInt* createBigInt(const SBigInt::BigInt &n) {
        return createExternalObject<SBigInt>(n);
    }

private:
    template<typename DerivedT, typename ...ArgT>
    DerivedT* createExternalObject(ArgT&& ...args) {
        if (mExternalObjCount >= mExternalObjThreshold) {
            performFullGC();
            return createExternalObject<DerivedT>(forward<ArgT>(args)...);
        }
        ++mExternalObjCount;

        auto p = new DerivedT(forward<ArgT>(args)...);

        p->next = mFirstExternalObj;
        mFirstExternalObj = p;

        ASSERT(force_cast<PtrValue>(p) % PTR_ALIGNMENT == 0);
        return p;
    }

    template<typename DerivedT, typename ...ArgT>
    DerivedT* createObject(ArgT&& ...args) {
        int requireBytes = DerivedT::estimateAlignedSize(forward<ArgT>(args)...) * SObject::ALIGNMENT;

        if (mFreeOfWorkingHeap + requireBytes > (int)mWorkingHeap.size()) {
            performFullGC();
            return createObject<DerivedT>(forward<ArgT>(args)...);
        }

        DerivedT *p = new (&mWorkingHeap[mFreeOfWorkingHeap]) DerivedT(forward<ArgT>(args)...);
        mFreeOfWorkingHeap += requireBytes;

        ASSERT(force_cast<PtrValue>(p) % PTR_ALIGNMENT == 0);
        return p;
    }

public:
    void installGCRootCollector(function<void(SObjectManager*)> f) {
        mGCRootCollector = f;
    }

    void mark(SValue *v);

    void mark(SExternalObject *obj);

    template<typename DerivedT>
    void mark(DerivedT **obj, typename enable_if<is_base_of<SObject, DerivedT>::value, void>::type* =0) {
        if (*obj) {
            *obj = getForwardedObject(static_cast<SObject*>(*obj))->staticCast<DerivedT>();
        }
    }

    void performFullGC();

private:
    SObject* getForwardedObject(SObject *obj);

private:
    int mExternalObjCount;
    int mExternalObjThreshold;
    SExternalObject *mFirstExternalObj;

    vector<char> mWorkingHeap;
    vector<char> mFreeHeap;
    int mFreeOfWorkingHeap;
    int mFreeOfFreeHeap;

    SSymbolManager *mSymbolMgr;

    function<void(SObjectManager*)> mGCRootCollector;
    queue<SExternalObject*> mMarkedExternalObjs;
};

#endif
