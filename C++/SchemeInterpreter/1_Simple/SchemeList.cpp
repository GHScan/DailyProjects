#include "pch.h"
#include "SchemeList.h"

SchemeList* SchemeList::create(SchemeMemoryManager *mgr, const SchemeRef *begin, const SchemeRef *end) {
    SchemeList *l = EMPTY;

    for (; end > begin; --end) {
        l = (SchemeList*)SchemePair::cons(mgr, end[-1], SchemeRef(l));
    }

    return l;
}

SchemeList* SchemeList::clone(SchemeMemoryManager *mgr) const {
    if (this == EMPTY) {
        return EMPTY;
    }

    const SchemeList *l = this;
    SchemePair *firstPair = SchemePair::create(mgr);
    firstPair->setCar(l->car());

    l = l->nextList();
    SchemePair *lastPair = firstPair;

    for (; l != EMPTY; l = l->nextList()) {
        SchemePair *newPair = SchemePair::create(mgr);
        lastPair->setCdr(SchemeRef(newPair));
        lastPair = newPair;
        lastPair->setCar(l->car());
    }

    lastPair->setCdr(SchemeRef(EMPTY));

    return (SchemeList*)firstPair;
}

SchemeList* SchemeList::pushFront(SchemeMemoryManager *mgr, const SchemeRef &v) {
    return (SchemeList*)SchemePair::cons(mgr, v, SchemeRef(this));
}

SchemeList* SchemeList::EMPTY = nullptr;
