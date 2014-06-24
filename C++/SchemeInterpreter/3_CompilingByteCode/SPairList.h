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
    SPairListBuilder(SObjectManager *mgr, SValue *r):
        mMgr(mgr), mLast(r) {
        *mLast = SValue::EMPTY;
    }

    SValue* alloc() {
        auto newPair = mMgr->createPair(mLast, SValue::EMPTY, SValue::EMPTY);
        mLast = &newPair->cdr;
        return &newPair->car;
    }

    void push(SValue v) {
        auto newPair = mMgr->createPair(mLast, v, SValue::EMPTY);
        mLast = &newPair->cdr;
    }

    void concat(SValue v) {
        *mLast = v;
        mLast = nullptr;
    }

private:
    SObjectManager *mMgr;
    SValue *mLast;
};

class SPairListAccessor {
public:
    class Iterator: public iterator<forward_iterator_tag, SValue> {
    public:
        Iterator(): mPair(nullptr) {
        }

        Iterator& operator ++ () {
            mPair = mPair->cdr == SValue::EMPTY ? nullptr : mPair->cdr.getObject()->staticCast<SPair>();
            return *this;
        }

        Iterator operator ++ (int) {
            Iterator iter = *this;
            ++*this;
            return iter;
        }

        SValue& operator * () {
            return mPair->car;
        }

        bool operator == (const Iterator &o) const {
            return mPair == o.mPair;
        }
        bool operator != (const Iterator &o) const {
            return !(*this == o);
        }

        SPair* getPair() {
            return mPair;
        }

    private:
        friend class SPairListAccessor;

        explicit Iterator(SPair *pair): mPair(pair) {
        }
    private:
        SPair *mPair;
    };

public:
    explicit SPairListAccessor(SPair *first):
        mFirst(first) {
    }

    template<int n>
    SValue& ref() {
        return PairHelp::get<n>(mFirst);
    }

    Iterator begin() {
        return Iterator(mFirst);
    }

    Iterator end() {
        return Iterator();
    }

private:
    SPair *mFirst;
};

#endif
