#ifndef SOBJECT_H
#define SOBJECT_H

struct SObject {
    SObject(const SObject&) = delete;
    SObject& operator = (const SObject&) = delete;

    bool mark() {
        bool b = !isMarked();
        mMarked = 1;
        return b;
    }

    bool isMarked() const {
        return mMarked == 1;
    }

    void unmark() {
        mMarked = 0;
    }

    int getType() const {
        return (int)mType;
    }

    bool equal(const SObject *o) const;
    void writeToStream(ostream &so) const;

    SObject* next;
protected:
    SObject(int type): next(nullptr), mType(type), mMarked(0) {
    }

    uint32_t mType : 31;
    uint32_t mMarked : 1;
};


#endif
