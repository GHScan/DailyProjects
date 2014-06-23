#ifndef SOBJECTMANAGER_H
#define SOBJECTMANAGER_H

#include "SValue.h"
#include "STypes.h"
#include "SSymbol.h"

class SObjectManager {
public:
    SObjectManager();
    ~SObjectManager();

    SObjectManager(const SObjectManager&) = delete;
    SObjectManager& operator = (const SObjectManager&) = delete;

    bool createBool(SValue *ret, bool b) {
        *ret = b ? SValue::TRUE : SValue::FALSE;
        return b;
    }

    int createInt(SValue *ret, int i) {
        ret->setInt(i);
        return i;
    }

    SSymbol* createSymbol(SValue *ret, const char *s) {
        auto sym = mSymbolMgr->getSymbol(s);
        ret->setSymbol(sym);
        return sym;
    }

    SDouble* createDouble(SValue *ret, double d) {
        return createObject<SDouble>(ret, d);
    }

    SString* createString(SValue *ret, const char *s) {
        return createObject<SString>(ret, s);
    }

    SPair* createPair(SValue *ret, SValue car, SValue cdr) {
        return createObject<SPair>(ret, car, cdr);
    }

    SEnv* createEnv(SValue *ret, SEnv *prevEnv, int localCount) {
        return createObject<SEnv>(ret, prevEnv, localCount);
    }

    SScriptFunction* createScriptFunction(SValue *ret, SScriptFunctionProto *proto, SEnv *env) {
        return createObject<SScriptFunction>(ret, proto, env);
    }

    SCFunction* createCFunction(SValue *ret, CFunction f) {
        return createExternalObject<SCFunction>(ret, f);
    }

    SBigInt* createBigInt(SValue *ret, const SBigInt::BigInt &n) {
        return createExternalObject<SBigInt>(ret, n);
    }

private:
    template<typename DerivedT, typename ...ArgT>
    DerivedT* createExternalObject(SValue *ret, ArgT&& ...args) {
        if (mExternalObjCount >= mExternalObjThreshold) {
            performFullGC();
            return createExternalObject<DerivedT>(ret, forward<ArgT>(args)...);
        }
        ++mExternalObjCount;

        auto p = new DerivedT(forward<ArgT>(args)...);
        ret->setExternalObject(p);

        p->next = mFirstExternalObj;
        mFirstExternalObj = p;

        return p;
    }

    template<typename DerivedT, typename ...ArgT>
    DerivedT* createObject(SValue *ret, ArgT&& ...args) {
        int requireBytes = DerivedT::estimateAlignedSize(forward<ArgT>(args)...) * SObject::ALIGNMENT;

        if (mFreeOfWorkingHeap + requireBytes > (int)mWorkingHeap.size()) {
            performFullGC();
            return createObject<DerivedT>(ret, forward<ArgT>(args)...);
        }

        DerivedT *p = new (&mWorkingHeap[mFreeOfWorkingHeap]) DerivedT(forward<ArgT>(args)...);
        mFreeOfWorkingHeap += requireBytes;
        ret->setObject(p);

        return p;
    }

public:
    void installGCRootCollector(function<void(SObjectManager*)> f) {
        mGCRootCollector = f;
    }

    SObject* mark(SObject *obj);
    void mark(SExternalObject *obj);

    void performFullGC();

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
