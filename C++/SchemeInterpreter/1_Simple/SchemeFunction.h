#ifndef SCHEME_FUNCTION_H
#define SCHEME_FUNCTION_H

#include "SchemeStaticObject.h"

class SchemeMemoryManager;
class SchemeEnv;
class SchemeList;

class SchemeFunction: public SchemeVector {
public:
    SchemeFunction() = delete;
    SchemeFunction(const SchemeFunction&) = delete;
    SchemeFunction& operator = (const SchemeFunction&) = delete;
    ~SchemeFunction() = delete;

    static SchemeFunction* create(SchemeMemoryManager *mgr, SchemeEnv *env, SchemeList *formals, SchemeList *bodyExps);

    SchemeEnv* getEnv();
    SchemeList* getFormals();
    SchemeList* getBodyExps();

private:
    enum {
        FIELD_Env,
        FIELD_Formals,
        FIELD_BodyExps,
    };
};

#endif
