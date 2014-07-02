#ifndef SOBJECTMANAGER_H
#define SOBJECTMANAGER_H

#include "SValue.h"
#include "STypes.h"

struct SFuncProto;
struct SClassProto;

class SObjectManager {
public:
    SObjectManager();
    ~SObjectManager();

    template<typename T, typename ...ArgT>
    T* createObject(ArgT && ...args) {

        int size = T::estimateSize(forward<ArgT>(args)...);

        if (mFreeOfWorkingHeap + size > (int)mWorkingHeap.size()) {
            mFreeHeap.resize(max((mFreeOfWorkingHeap + size) * 3 / 2, (int)mFreeHeap.size()));
            performFullGC();
        }
        ASSERT(mFreeOfWorkingHeap + size <= (int)mWorkingHeap.size());

        T *p = new (&mWorkingHeap[mFreeOfWorkingHeap]) T(forward<ArgT>(args)...);
        mFreeOfWorkingHeap += size;

        p->setSize(size);

        return p;
    }

    SObjectManager(const SObjectManager&) = delete;
    SObjectManager& operator = (const SObjectManager&) = delete;

public:
    void installRootCollector(function<void(SObjectManager*)> collector) {
        mRootCollector = collector;
    }

    void mark(SValue *v);
    SObject* mark(SObject *obj);

    template<typename T> 
    void mark(T ** p) {
        *p = static_cast<T*>(mark(*p));
    }

    void performFullGC();

private:
    function<void(SObjectManager*)> mRootCollector;
    vector<uint8_t> mWorkingHeap;
    int mFreeOfWorkingHeap;
    vector<uint8_t> mFreeHeap;
};

#endif
