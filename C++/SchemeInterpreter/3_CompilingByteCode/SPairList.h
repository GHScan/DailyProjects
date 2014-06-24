#ifndef SPAIRLIST_H
#define SPAIRLIST_H

#include "STypes.h"
#include "SObjectManager.h"

namespace PairHelp {

template<int n>
inline SValue& get(SPair *pair) {
    return get<n - 1>(pair->cdr.getObject()->staticCast<SPair>());
}

template<>
inline SValue& get<0>(SPair *pair) {
    return pair->car;
}

}

class SPairListBuilder {
public:
    SPairListBuilder(SObjectManager *mgr):
        mMgr(mgr), mList(SValue::EMPTY), mLastPair(nullptr) {
    }

    SValue getList() {
        return mList.value;
    }

    SPairListBuilder& push(SValue v) {
        auto newPair = mMgr->createPair(ScopedValue<SValue>(v));
        if (mList.value == SValue::EMPTY) {
            mList.value.setObject(newPair);
            mLastPair.value = newPair;
        } else {
            mLastPair.value->staticCast<SPair>()->cdr.setObject(newPair);
            mLastPair.value = newPair;
        }
        return *this;
    }

    void concat(SValue v) {
        if (mList.value == SValue::EMPTY) mList.value = v;
        else {
            mLastPair.value->staticCast<SPair>()->cdr = v;
        }
    }

    static void* operator new (size_t) = delete;
    static void operator delete (void*) = delete;

private:
    SObjectManager *mMgr;
    ScopedValue<SValue> mList;
    ScopedValue<SObject*> mLastPair;
};

class SPairListAccessor {
public:
    class Iterator: public iterator<forward_iterator_tag, SValue> {
    public:
        Iterator(): mPair(SValue::EMPTY) {
        }

        Iterator& operator ++ () {
            mPair.value = getPair()->cdr;
            return *this;
        }

        Iterator operator ++ (int) {
            Iterator iter = *this;
            ++*this;
            return iter;
        }

        SValue& operator * () {
            return getPair()->car;
        }

        bool operator == (const Iterator &o) const {
            return mPair.value == o.mPair.value;
        }
        bool operator != (const Iterator &o) const {
            return !(*this == o);
        }

        SPair* getPair() {
            return mPair.value.getObject()->staticCast<SPair>();
        }

    private:
        friend class SPairListAccessor;

        explicit Iterator(SValue pair): mPair(pair) {
        }
    private:
        ScopedValue<SValue> mPair;
    };

public:
    explicit SPairListAccessor(SValue first):
        mFirst(first) {
    }

    template<int n>
    SValue& ref() {
        return PairHelp::get<n>(mFirst.value.getObject()->staticCast<SPair>());
    }

    Iterator begin() {
        return Iterator(mFirst.value);
    }

    Iterator end() {
        return Iterator();
    }

    static void* operator new (size_t) = delete;
    static void operator delete (void*) = delete;

private:
    ScopedValue<SValue> mFirst;
};

#endif
