#include "pch.h"
#include "SchemeFunction.h"
#include "SchemeMemoryManager.h"
#include "SchemeEnv.h"
#include "SchemeList.h"

SchemeFunction* SchemeFunction::create(SchemeMemoryManager *mgr, SchemeEnv *env, SchemeList *formals, SchemeList *bodyExps) {
    SchemeFunction *p = static_cast<SchemeFunction*>(SchemeVector::create(mgr, 3));
    sref<FIELD_Env>(p) = env;
    sref<FIELD_Formals>(p) = formals;
    sref<FIELD_BodyExps>(p) = bodyExps;
    return p;
}

SchemeEnv* SchemeFunction::getEnv() {
    return sref<FIELD_Env>(this).getStaticObject()->castToVector<SchemeEnv>();
}

SchemeList* SchemeFunction::getFormals() {
    return sref<FIELD_Formals>(this).getStaticObject()->castToPair<SchemeList>();
}

SchemeList* SchemeFunction::getBodyExps() {
    return sref<FIELD_BodyExps>(this).getStaticObject()->castToPair<SchemeList>();
}
