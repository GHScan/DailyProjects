#ifndef SCHEME_LIST_H
#define SCHEME_LIST_H

#include "SchemeRef.h"
#include "SchemeStaticObject.h"

class SchemeMemoryManager;

class SchemeList: public SchemePair {
public:
    SchemeList() = delete;
    SchemeList(const SchemeList &o) = delete;
    SchemeList& operator = (const SchemeList &o) = delete;
    ~SchemeList() = delete;

    static SchemeList* create(SchemeMemoryManager *mgr, const SchemeRef *begin, const SchemeRef *end);

    const SchemeList* nextList() const {
        return cdr().getStaticObject()->castToPair<SchemeList>();
    }

    bool empty(const SchemeRef &v) const {
        return v.getPointer() == EMPTY;
    }

    SchemeList* clone(SchemeMemoryManager *mgr) const;

    const SchemePair* lastPair() const {
        assert(this != EMPTY);

        const SchemeList *l = this;
        for (const SchemeList *next = l->nextList(); next != EMPTY; next = l->nextList()) {
            l = next;
        }
        return l;
    }
    SchemePair* lastPair() {
        return const_cast<SchemePair*>(const_cast<const SchemeList*>(this)->lastPair());
    }

    SchemeList* append(SchemeMemoryManager *mgr, SchemeList *l) const {
        if (this == EMPTY) {
            return l->clone(mgr);
        } else {
            SchemeList *newl = clone(mgr);
            newl->mappend(l);
            return newl;
        }
    }

    void mappend(SchemeList *l) {
        if (this != EMPTY) {
            lastPair()->setCdr(l);
        }
    }

    SchemeList* pushFront(SchemeMemoryManager *mgr, const SchemeRef &v);

    const SchemeList* member(const SchemeRef &v) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().equal(v); l = l->nextList());
        return l;
    }
    SchemeList* member(const SchemeRef &v) {
        return const_cast<SchemeList*>(const_cast<const SchemeList*>(this)->member(v));
    }

    const SchemeList* memv(const SchemeRef &v) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().eqv(v); l = l->nextList());
        return l;
    }
    SchemeList* memv(const SchemeRef &v) {
        return const_cast<SchemeList*>(const_cast<const SchemeList*>(this)->memv(v));
    }

    const SchemeList* memq(const SchemeRef &v) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().eq(v); l = l->nextList());
        return l;
    }
    SchemeList* memq(const SchemeRef &v) {
        return const_cast<SchemeList*>(const_cast<const SchemeList*>(this)->memq(v));
    }

    const SchemePair* assoc(const SchemeRef &k) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().getStaticObject()->castToPair<SchemePair>()->car().equal(k); l = l->nextList());
        return l->car().getStaticObject()->castToPair<SchemePair>();
    }
    SchemePair* assoc(const SchemeRef &v) {
        return const_cast<SchemePair*>(const_cast<const SchemeList*>(this)->assoc(v));
    }

    const SchemePair* assv(const SchemeRef &k) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().getStaticObject()->castToPair<SchemePair>()->car().eqv(k); l = l->nextList());
        return l->car().getStaticObject()->castToPair<SchemePair>();
    }
    SchemePair* assv(const SchemeRef &v) {
        return const_cast<SchemePair*>(const_cast<const SchemeList*>(this)->assv(v));
    }

    const SchemePair* assq(const SchemeRef &k) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().getStaticObject()->castToPair<SchemePair>()->car().eq(k); l = l->nextList());
        return l->car().getStaticObject()->castToPair<SchemePair>();
    }
    SchemePair* assq(const SchemeRef &v) {
        return const_cast<SchemePair*>(const_cast<const SchemeList*>(this)->assq(v));
    }

    int length() const {
        int n = 0;

        for (const SchemeList *l = this; l != EMPTY; l = l->nextList()) {
            ++n;
        }

        return n;
    }

    const SchemeRef& ref(int i) const {
        const SchemeList *l = this;
        while (i-- > 0) l = l->nextList();

        return l->car();
    }

    static void installEmptyList(SchemeList *l) {
        EMPTY = l;
    }

    static SchemeList* EMPTY;
};

template<int n>
inline const SchemeRef& sref(const SchemeList *l) {
    return sref<n - 1>(l->nextList());
}
template<>
inline const SchemeRef& sref<0>(const SchemeList *l) {
    return l->car();
}

template<int n>
inline SchemeRef& sref(SchemeList *l) {
    return const_cast<SchemeRef&>(sref<n>(const_cast<const SchemeList*>(l)));
}
 
#endif
