#include "pch.h"
#include "SchemeEnv.h"
#include "SchemeMemoryManager.h"
#include "SchemeList.h"
#include "SchemeSymbol.h"

SchemeEnv* SchemeEnv::create(SchemeMemoryManager *mgr, SchemeEnv *prev, SchemeList *formals, SchemeList *actuals) {
    SchemeEnv *env = static_cast<SchemeEnv*>(SchemeVector::create(mgr, 3));
    sref<FIELD_PrevEnv>(env) = prev;
    sref<FIELD_Names>(env) = formals;
    sref<FIELD_Values>(env) = actuals;
    return env;
}

const SchemeRef* SchemeEnv::lookup(const SchemeSymbol &name) const {
    if (this == nullptr) return nullptr;

    const SchemeList *names = sref<FIELD_Names>(this).getStaticObject()->castToPair<SchemeList>();
    const SchemeList *values = sref<FIELD_Values>(this).getStaticObject()->castToPair<SchemeList>();
    for (; names != SchemeList::EMPTY && *names->car().getSymbol() != name; names = names->nextList(), values = values->nextList());

    if (names == SchemeList::EMPTY) {
        return sref<FIELD_PrevEnv>(this).getStaticObject()->castToVector<SchemeEnv>()->lookup(name);
    } 

    return &values->car();
}

void SchemeEnv::define(SchemeMemoryManager *mgr, const SchemeSymbol &name, const SchemeRef &v) {
    sref<FIELD_Names>(this) = sref<FIELD_Names>(this).getStaticObject()->castToPair<SchemeList>()->pushFront(mgr, &name);
    sref<FIELD_Values>(this) = sref<FIELD_Values>(this).getStaticObject()->castToPair<SchemeList>()->pushFront(mgr, v);
}
