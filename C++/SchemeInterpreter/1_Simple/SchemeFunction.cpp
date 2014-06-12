#include "pch.h"
#include "SchemeFunction.h"
#include "SchemeMemoryManager.h"
#include "SchemeEnv.h"
#include "SchemeList.h"

SchemeFunction* SchemeFunction::create(SchemeMemoryManager *mgr, SchemeEnv *env, SchemeList *formals, SchemeList *bodyExps) {
    SchemeFunction *p = (SchemeFunction*)SchemeVector::create(mgr, 3);
    p->set(FIELD_Env, SchemeRef(env));
    p->set(FIELD_Formals, SchemeRef(formals));
    p->set(FIELD_BodyExps, SchemeRef(bodyExps));
    return p;
}
