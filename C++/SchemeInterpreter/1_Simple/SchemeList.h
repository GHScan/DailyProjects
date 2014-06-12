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
        SchemePair *p = (SchemePair*)cdr().getStaticObject();
        assert(p->isPair());
        return (SchemeList*)p;
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
        return (SchemePair*)((const SchemeList*)this)->lastPair();
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
            lastPair()->setCdr(SchemeRef(l));
        }
    }

    SchemeList* pushFront(SchemeMemoryManager *mgr, const SchemeRef &v);

    const SchemeList* member(const SchemeRef &v) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().equal(v); l = l->nextList());
        return l;
    }
    SchemeList* member(const SchemeRef &v) {
        return (SchemeList*)((const SchemeList*)this)->member(v);
    }

    const SchemeList* memv(const SchemeRef &v) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().eqv(v); l = l->nextList());
        return l;
    }
    SchemeList* memv(const SchemeRef &v) {
        return (SchemeList*)((const SchemeList*)this)->memv(v);
    }

    const SchemeList* memq(const SchemeRef &v) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !l->car().eq(v); l = l->nextList());
        return l;
    }
    SchemeList* memq(const SchemeRef &v) {
        return (SchemeList*)((const SchemeList*)this)->memq(v);
    }

    const SchemePair* assoc(const SchemeRef &k) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !((SchemePair*)l->car().getStaticObject())->car().equal(k); l = l->nextList());
        return (SchemePair*)l->car().getStaticObject();
    }
    SchemePair* assoc(const SchemeRef &v) {
        return (SchemePair*)((const SchemeList*)this)->assoc(v);
    }

    const SchemePair* assv(const SchemeRef &k) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !((SchemePair*)l->car().getStaticObject())->car().eqv(k); l = l->nextList());
        return (SchemePair*)l->car().getStaticObject();
    }
    SchemePair* assv(const SchemeRef &v) {
        return (SchemePair*)((const SchemeList*)this)->assv(v);
    }

    const SchemePair* assq(const SchemeRef &k) const {
        const SchemeList *l = this;
        for (; l != EMPTY && !((SchemePair*)l->car().getStaticObject())->car().eq(k); l = l->nextList());
        return (SchemePair*)l->car().getStaticObject();
    }
    SchemePair* assq(const SchemeRef &v) {
        return (SchemePair*)((const SchemeList*)this)->assq(v);
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

    const SchemeRef& first() const {
        return car();
    }
    SchemeRef& first() {
        return (SchemeRef&)((const SchemeList*)this)->first();
    }

    const SchemeRef& second() const {
        return nextList()->car();
    }
    SchemeRef& second() {
        return (SchemeRef&)((const SchemeList*)this)->second();
    }

    const SchemeRef& third() const {
        return nextList()->nextList()->car();
    }
    SchemeRef& third() {
        return (SchemeRef&)((const SchemeList*)this)->third();
    }

    static void installEmptyList(SchemeList *l) {
        EMPTY = l;
    }

    static SchemeList* EMPTY;
};

#endif
