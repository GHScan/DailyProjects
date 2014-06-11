#ifndef TAGGED_POINTER_H
#define TAGGED_POINTER_H

class TaggedPointer {
public:
    static const PtrValue TAG_BIT_COUNT = 3;
    static const PtrValue TAG_MAX = 1 << TAG_BIT_COUNT;
    static const PtrValue TAG_MASK = TAG_MAX - 1;

public:
    static const PtrValue MAX_ULONG = 1L << (sizeof(void*) * 8 - TAG_BIT_COUNT);
    static const PtrValue MIN_ULONG = 0;
    static const PtrValue MAX_LONG = MAX_ULONG / 2 - 1;
    static const PtrValue MIN_LONG = -(MAX_ULONG / 2);

public:
    TaggedPointer() {}

    TaggedPointer(PtrValue tag, void* p) {
        set(tag, p);
    }

    void set(PtrValue tag, void* p) {
        assert(((PtrValue)p & TAG_MASK) == 0);
        assert((tag & TAG_MASK) == tag);
        mPointer = tag | (PtrValue)p;
    }

    PtrValue getTag() const {
        return mPointer & TAG_MASK;
    }

    void* getPointer() const {
        return (void*)(mPointer & ~TAG_MASK);
    }

private:
    PtrValue mPointer;
};


#endif
