#include "pch.h"
#include "SchemeEnv.h"
#include "SchemeMemoryManager.h"
#include "SchemeList.h"
#include "SchemeSymbol.h"

SchemeEnv* SchemeEnv::create(SchemeMemoryManager *mgr, SchemeEnv *prev, SchemeList *formals, SchemeList *actuals) {
    SchemeEnv *env = (SchemeEnv*)SchemeVector::create(mgr, 3);
    env->set(FIELD_PrevEnv, SchemeRef(prev));
    env->set(FIELD_Names, SchemeRef(formals));
    env->set(FIELD_Values, SchemeRef(actuals));
    return env;
}

const SchemeRef* SchemeEnv::lookup(const SchemeSymbol &name) const {
    if (this == nullptr) return nullptr;

    const SchemeList *names = (SchemeList*)ref(FIELD_Names).getStaticObject();
    const SchemeList *values = (SchemeList*)ref(FIELD_Values).getStaticObject();
    for (; names != SchemeList::EMPTY && *names->car().getSymbol() != name; names = names->nextList(), values = values->nextList());

    if (names == SchemeList::EMPTY) {
        return ((SchemeEnv*)ref(FIELD_PrevEnv).getStaticObject())->lookup(name);
    } 

    return &values->car();
}

void SchemeEnv::define(SchemeMemoryManager *mgr, const SchemeSymbol &name, const SchemeRef &v) {
    set(FIELD_Names, SchemeRef(((SchemeList*)ref(FIELD_Names).getStaticObject())->pushFront(mgr, SchemeRef(&name))));
    set(FIELD_Values, SchemeRef(((SchemeList*)ref(FIELD_Values).getStaticObject())->pushFront(mgr, v)));
}
