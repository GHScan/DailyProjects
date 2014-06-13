#ifndef SCHEME_STATIC_OBJECT_H
#define SCHEME_STATIC_OBJECT_H

#include "SchemeRef.h"

class SchemeMemoryManager;
struct SchemePair;
struct SchemeVector;

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

    template<typename T>
    const T* castToPair() const {
        assert(isPair());

        return static_cast<const T*>(static_cast<const SchemePair*>(this));
    }
    template<typename T>
    T* castToPair() {
        return const_cast<T*>(const_cast<const SchemeStaticObject*>(this)->castToPair<T>());
    }

    template<typename T>
    const T* castToVector() const {
        assert(isVector());

        return static_cast<const T*>(static_cast<const SchemeVector*>(this));
    }
    template<typename T>
    T* castToVector() {
        return const_cast<T*>(const_cast<const SchemeStaticObject*>(this)->castToVector<T>());
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

    int length() const {
        return first.getInteger();
    }

    const SchemeRef& ref(int i) const {
        assert(i >= 0 && i < length());
        return (&second)[i];
    }
    SchemeRef& ref(int i) {
        return (SchemeRef&)((const SchemeVector*)this)->ref(i);
    }

private:
    SchemeVector(int size): SchemeStaticObject(TYPE_Vector) {
        first.setInteger(size);
    }
};

template<int n>
inline const SchemeRef& sref(const SchemeVector *v) {
    assert(n < v->length());

    return (&v->second)[n];
}
template<>
inline const SchemeRef& sref<0>(const SchemeVector *v) {
    return v->second;
}

template<int n>
inline SchemeRef& sref(SchemeVector *v) {
    return const_cast<SchemeRef&>(sref<n>(const_cast<const SchemeVector*>(v)));
}

#endif
