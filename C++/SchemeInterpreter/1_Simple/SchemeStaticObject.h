#ifndef SCHEME_STATIC_OBJECT_H
#define SCHEME_STATIC_OBJECT_H

#include "SchemeRef.h"

class SchemeMemoryManager;

struct SchemeStaticObject {
    enum {
        TYPE_Pair,
        TYPE_Vector,
    };

    SchemeStaticObject(const SchemeStaticObject& o) = delete;
    SchemeStaticObject& operator = (const SchemeStaticObject& o) = delete;
    ~SchemeStaticObject() = default;

    bool isPair() const {
        return first.getTag() == TYPE_Pair;
    }

    bool isVector() const {
        return first.getTag() == TYPE_Vector;
    }

    int getRefCount() const {
        return isPair() ? 2 : 1 + first.getInteger();
    }

    const SchemeRef& operator [] (int i) const {
        assert(i >= 0 && i < getRefCount());

        return (&first)[i];
    }
    SchemeRef& operator [] (int i) {
        return (SchemeRef&)(*(const SchemeStaticObject*)this)[i];
    }

    bool equal(const SchemeStaticObject &o) const {
        int refCount = getRefCount();
        if (refCount != o.getRefCount()) return false;

        for (int i = 0; i < refCount; ++i) {
            if (!(*this)[i].equal(o[i])) return false;
        }

        return true;
    }

    SchemeRef first; 
    SchemeRef second;

protected:
    explicit SchemeStaticObject(PtrValue type) {
        first.setTag(type);
    }
};

struct SchemePair: public SchemeStaticObject {

    static SchemePair* create(SchemeMemoryManager *mgr);

    static SchemePair* cons(SchemeMemoryManager* mgr, const SchemeRef &car,  const SchemeRef &cdr) {
        SchemePair *p = create(mgr);
        p->setCar(car);
        p->setCdr(cdr);
        return p;
    }

    const SchemeRef& car() const {
        return first;
    }

    const SchemeRef& cdr() const {
        return second;
    }

    void setCar(const SchemeRef &v) {
        first = v;
    }

    void setCdr(const SchemeRef &v) {
        second = v;
    }
private:
    SchemePair(): SchemeStaticObject(TYPE_Pair) {
    }
};
static_assert(sizeof(SchemePair) == sizeof(void*) * 2, "SchemePair is the core structure of scheme, it should be compacted enough!");

struct SchemeVector: public SchemeStaticObject {
    static SchemeVector* create(SchemeMemoryManager *mgr, int size);

    int size() const {
        return first.getInteger();
    }

    const SchemeRef& ref(int i) const {
        assert(i >= 0 && i < size());

        return (&second)[i];
    }

    void set(int i, const SchemeRef &v) {
        (SchemeRef&)ref(i) = v;
    }

private:
    SchemeVector(int size): SchemeStaticObject(TYPE_Vector) {
        first.setInteger(size);
    }
};

#endif
