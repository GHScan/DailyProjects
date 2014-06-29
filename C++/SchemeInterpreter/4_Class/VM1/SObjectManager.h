#ifndef SOBJECTMANAGER_H
#define SOBJECTMANAGER_H

#include "SValue.h"
#include "STypes.h"
#include "MemoryPool.h"

struct SFuncProto;
struct SClassProto;

class SObjectManager {
public:
    SObjectManager(int initGCThreshold);
    ~SObjectManager();

    void createBool(SValue *ret, bool b) {
        *ret = b ? SValue::TRUE : SValue::FALSE;
    }

    void createNumber(SValue *ret, double number) {
        ret->setNumber(number);
    }

    void createString(SValue *ret, Atom* atom) {
        ret->setString(atom);
    }

    void createSymbol(SValue *ret, Atom* atom) {
        ret->setSymbol(atom);
    }

    template<typename T, typename ...ArgT>
    T* createObject(SValue *ret, ArgT && ...args) {
        if (mMemPool.getMemorySize() > mGCThreshold) {
            performFullGC();
        }

        auto p = new (mMemPool.malloc(T::estimateSize(forward<ArgT>(args)...), alignof(T))) T(forward<ArgT>(args)...);
        ret->setObject(p);

        p->next = mFirstObj;
        mFirstObj = p;

        return p;
    }

    template<typename T>
    void destroyObject(T *p) {
        int size = p->estimateSize();
        p->~T();
        mMemPool.free(p, size, alignof(T));
    }

    SObjectManager(const SObjectManager&) = delete;
    SObjectManager& operator = (const SObjectManager&) = delete;

public:
    void installRootCollector(function<void(SObjectManager*)> collector) {
        mRootCollector = collector;
    }

    void mark(SValue v);
    void mark(SObject *obj);

    void performFullGC();

private:
    MemoryPool mMemPool;
    function<void(SObjectManager*)> mRootCollector;
    SObject *mFirstObj;
    uint64_t mGCThreshold;
    queue<SObject*> mMarkedObjs;
};

#endif
