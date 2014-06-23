#ifndef SOBJECT_H
#define SOBJECT_H

#include "TaggedPointer.h"

class SValue;

// the derived class of SObject can only have POD member, if you want to a non-POD member, derived from SExternalObject

class SObject {
public:
    int getType() const {
        return mType;
    }

    int getAlignedSize() const {
        return mAlignedSize;
    }

    bool isRedirected() const {
        return mRedirected == 1;
    }

    void* getRedirectedPtr() {
        return *reinterpret_cast<void**>(&this[1]);
    }

    void redirect(void *p) {
        mRedirected = 1;
        *reinterpret_cast<void**>(&this[1]) = p;
    }

    template<typename DerivedT>
    DerivedT* staticCast() {
        ASSERT(getType() == DerivedT::TYPE);
        return static_cast<DerivedT*>(this);
    }

    SObject(const SObject&) = delete;
    SObject& operator = (const SObject&) = delete;

    static const int ALIGNMENT = PTR_ALIGNMENT;
    static int objectSizeToAlignedSize(int bytes) {
        return roundUp<ALIGNMENT>(max(bytes, int(sizeof(SObject) + sizeof(void*)))) / ALIGNMENT;
    }

    SObject(int type, int alignedSize):
        mType(type), mRedirected(0), mAlignedSize(alignedSize) {
    }

private:
    uint32_t mType : 5;
    uint32_t mRedirected : 1;
    uint32_t mAlignedSize : 26;
};

class SExternalObject {
public:
    bool isMarked() const {
        return mMarked == 1;
    }

    bool mark() {
        bool b = mMarked == 0;
        mMarked = 1;
        return b;
    }

    void unmark() {
        mMarked = 0;
    }

    int getType() const {
        return mType;
    }

    template<typename DerivedT>
    DerivedT* staticCast() {
        ASSERT(getType() == DerivedT::TYPE);
        return static_cast<DerivedT*>(this);
    }

    SExternalObject(const SExternalObject&) = delete;
    SExternalObject& operator = (const SExternalObject&) = delete;

    SExternalObject *next;
protected:
    explicit SExternalObject(int type): 
        next(nullptr), mMarked(0), mType(type) {
        ASSERT(type >= 0 && type < 32);
    }

private:
    uint32_t mMarked : 1;
    uint32_t mType : 5;
};

#endif
