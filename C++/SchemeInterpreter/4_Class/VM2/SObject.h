#ifndef SOBJECT_H
#define SOBJECT_H

struct SObject {
    SObject(const SObject&) = delete;
    SObject& operator = (const SObject&) = delete;

    bool equal(const SObject *o) const;
    void writeToStream(ostream &so) const;

    int getType() const {
        return (int)mType;
    }

    bool isForwarded() const {
        return mForwarded == 1;
    }

    SObject* getForwardPtr() {
        return *reinterpret_cast<SObject**>(&this[1]);
    }

    void forward(SObject *newPtr) {
        mForwarded = 1;
        *reinterpret_cast<SObject**>(&this[1]) = newPtr;
    }

    void setSize(int size) {
        ASSERT(mSize == 0);
        mSize = size;
    }

    int getSize() const {
        return (int)mSize;
    }

protected:
    SObject(int type): mType(type), mForwarded(0), mSize(0) {
    }

    static int toAlignedSize(int bytes) {
        return (max(bytes, int(sizeof(SObject) + sizeof(SObject*))) + PTR_ALIGNMENT - 1) / PTR_ALIGNMENT * PTR_ALIGNMENT;
    }

    uint32_t mType : 5;
    uint32_t mForwarded : 1;
    uint32_t mSize : 20;
};


#endif
