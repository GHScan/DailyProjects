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
        *ret = SValue(number);
    }

    void createString(SValue *ret, Atom* atom) {
        ret->setString(atom);
    }

    void createSymbol(SValue *ret, Atom* atom) {
        ret->setSymbol(atom);
    }

    template<typename T, typename ...ArgT>
    T* createObject(SValue *ret, ArgT && ...args) {
        for (int i = GEN_N - 1; i >= 0; --i) {
            if (mGens[i].size > mGens[i].threshold) {
                performGC(i);
                break;
            }
        }

        auto youngGen = &mGens[0];

        int size = T::estimateSize(forward<ArgT>(args)...);
        youngGen->size += size;

        auto p = new (mMemPool.malloc(size, alignof(T))) T(forward<ArgT>(args)...);
        *ret = SValue(p);

        p->next = youngGen->firstObj;
        youngGen->firstObj = p;

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
    bool isOldGenForGC(SObject *obj);

    void performGC(int gen);

private:
    struct GenerationInfo {
        SObject *firstObj;
        uint64_t size;
        uint64_t threshold;
        uint64_t initThreshold;
    };

    static const int GEN_N = 4;
    static const int AGE_2_GEN[SObject::MAX_AGE + 1];
    static const int GEN_2_AGE[GEN_N];

private:
    MemoryPool mMemPool;
    function<void(SObjectManager*)> mRootCollector;
    vector<SObject*> mMarkedObjs;
    int mGCAge;
    GenerationInfo mGens[GEN_N];
};

#endif
