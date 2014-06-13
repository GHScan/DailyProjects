#include "pch.h"
#include "SchemeList.h"

SchemeList* SchemeList::create(SchemeMemoryManager *mgr, const SchemeRef *begin, const SchemeRef *end) {
    SchemeList *l = EMPTY;

    for (; end > begin; --end) {
        l = static_cast<SchemeList*>(SchemePair::cons(mgr, end[-1], l));
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
        lastPair->setCdr(newPair);
        lastPair = newPair;
        lastPair->setCar(l->car());
    }

    lastPair->setCdr(EMPTY);

    return static_cast<SchemeList*>(firstPair);
}

SchemeList* SchemeList::pushFront(SchemeMemoryManager *mgr, const SchemeRef &v) {
    return static_cast<SchemeList*>(SchemePair::cons(mgr, v, this));
}

SchemeList* SchemeList::EMPTY = nullptr;
