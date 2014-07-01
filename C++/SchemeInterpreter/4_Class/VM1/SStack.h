#ifndef SSTACK_H
#define SSTACK_H

#include "SValue.h"

class SEvalStack {
public:
    SEvalStack(int initCapacity) {
        ASSERT(initCapacity > 0);

        mBase = static_cast<SValue*>(::malloc(sizeof(SValue) * initCapacity));
        mTop = mBase;
        mCapacity = mBase + initCapacity;
    }

    ~SEvalStack() {
        FREE(mBase);
        mTop = mCapacity = nullptr;
    }

    SEvalStack(const SEvalStack&) = delete;
    SEvalStack& operator = (const SEvalStack&) = delete;

    void reserve(int capacity) {
        if (mCapacity - mBase < capacity) {
            capacity = max(capacity, int(mCapacity - mBase) * 3 / 2);

            int size = mTop - mBase;

            mBase = static_cast<SValue*>(::realloc(mBase, sizeof(SValue) * capacity));
            mTop = mBase + size;
            mCapacity = mBase + capacity;
        }
    }

    const SValue& operator [] (int i) const {
        ASSERT(i >= 0 && i < mTop - mBase);
        return mBase[i];
    }

    SValue& operator [] (int i) {
        return const_cast<SValue&>((*const_cast<const SEvalStack*>(this))[i]);
    }

    void push(SValue v) {
        ASSERT(mTop + 1 <= mCapacity);

        *mTop++ = v;
    }

    template<int i>
    SValue& top() {
        static_assert(i < 0, "");
        return mTop[i];
    }

    SValue pop() {
        return *--mTop;
    }

    void popn(int n) {
        mTop -= n;
    }

    bool empty() const {
        return mBase == mTop;
    }

    int size() const {
        return mTop - mBase;
    }

    SValue* begin() {
        return mBase;
    }

    SValue* end() {
        return mTop;
    }

private:
    SValue *mTop;
    SValue *mBase;
    SValue *mCapacity;
};

#endif
