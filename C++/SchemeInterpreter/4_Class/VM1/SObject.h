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

    int getAge() const {
        return mAge;
    }

    void growUp() {
        mAge = mAge < MAX_AGE ? mAge + 1 : mAge;
    }

    SObject* next;

    static const int MAX_AGE = 8;
    static vector<SObject*> sYoungContainer;

protected:
    SObject(int type): next(nullptr), mType(type), mMarked(0), mAge(0) {
    }

    uint32_t mType : 5;
    uint32_t mMarked : 1;
    uint32_t mAge : 10;
};


#endif
