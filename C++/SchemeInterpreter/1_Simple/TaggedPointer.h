#ifndef TAGGED_POINTER_H
#define TAGGED_POINTER_H

class TaggedPointer {
public:
    static const int TAG_BIT_COUNT = 3;
    static const PtrValue TAG_MAX = 1L << TAG_BIT_COUNT;
    static const PtrValue TAG_MASK = TAG_MAX - 1;

public:
    static const int POINTER_BIT_COUNT = (sizeof(PtrValue) * 8) - TAG_BIT_COUNT;
    static const PtrValue POINTER_MASK = ((1L << POINTER_BIT_COUNT) - 1) << TAG_BIT_COUNT;

public:
    static const PtrValue MAX_ULONG = (1L << POINTER_BIT_COUNT) - 1;
    static const PtrValue MIN_ULONG = 0;
    static const PtrValue MAX_LONG = MAX_ULONG / 2;
    static const PtrValue MIN_LONG = -(MAX_ULONG / 2 + 1);

public:
    template<PtrValue MASK>
    PtrValue get() const {
        return mPointer & MASK;
    }

    template<PtrValue MASK>
    void set(PtrValue v) {
        assert((v & MASK) == v);

        mPointer = v | (mPointer & ~MASK);
    }

    void* getPointer() const {
        return (void*)get<POINTER_MASK>();
    }

    void setPointer(void *p) {
        set<POINTER_MASK>((PtrValue)p);
    }

protected:
    PtrValue mPointer;
};


#endif
