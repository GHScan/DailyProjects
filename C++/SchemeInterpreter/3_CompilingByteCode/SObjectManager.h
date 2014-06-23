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

    bool createBool(SValue *r, bool b) {
        *r = b ? SValue::TRUE : SValue::FALSE;
        return b;
    }

    int createInt(SValue *r, int i) {
        r->setInt(i);
        return i;
    }

    SSymbol* createSymbol(SValue *r, const char *s) {
        auto sym = mSymbolMgr->getSymbol(s);
        r->setSymbol(sym);
        return sym;
    }

    SDouble* createDouble(SValue *r, double d) {
        return createObject<SDouble>(r, d);
    }

    SString* createString(SValue *r, const char *s) {
        return createObject<SString>(r, s);
    }

    SPair* createPair(SValue *r, SValue car, SValue cdr) {
        return createObject<SPair>(r, car, cdr);
    }

    SEnv* createEnv(SValue *r, SEnv *prevEnv, int localCount) {
        return createObject<SEnv>(r, prevEnv, localCount);
    }

    SScriptFunction* createScriptFunction(SValue *r, SScriptFunctionProto *proto, SEnv *env) {
        return createObject<SScriptFunction>(r, proto, env);
    }

    SCFunction* createCFunction(SValue *r, CFunction f) {
        return createExternalObject<SCFunction>(r, f);
    }

    SBigInt* createBigInt(SValue *r, const SBigInt::BigInt &n) {
        return createExternalObject<SBigInt>(r, n);
    }

private:
    template<typename DerivedT, typename ...ArgT>
    DerivedT* createExternalObject(SValue *r, ArgT&& ...args) {
        if (mExternalObjCount >= mExternalObjThreshold) {
            performFullGC();
            return createExternalObject<DerivedT>(r, forward<ArgT>(args)...);
        }
        ++mExternalObjCount;

        auto p = new DerivedT(forward<ArgT>(args)...);
        r->setExternalObject(p);

        p->next = mFirstExternalObj;
        mFirstExternalObj = p;

        return p;
    }

    template<typename DerivedT, typename ...ArgT>
    DerivedT* createObject(SValue *r, ArgT&& ...args) {
        int requireBytes = DerivedT::estimateAlignedSize(forward<ArgT>(args)...) * SObject::ALIGNMENT;

        if (mFreeOfWorkingHeap + requireBytes > (int)mWorkingHeap.size()) {
            performFullGC();
            return createObject<DerivedT>(r, forward<ArgT>(args)...);
        }

        DerivedT *p = new (&mWorkingHeap[mFreeOfWorkingHeap]) DerivedT(forward<ArgT>(args)...);
        mFreeOfWorkingHeap += requireBytes;
        r->setObject(p);

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
