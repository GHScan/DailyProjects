#ifndef SCHEME_ENV_H
#define SCHEME_ENV_H

#include "SchemeStaticObject.h"

class SchemeList;
class SchemeSymbol;
class SchemeMemoryManager;

class SchemeEnv: public SchemeVector {
public:
    SchemeEnv() = delete;
    SchemeEnv(const SchemeEnv&) = delete;
    SchemeEnv& operator = (const SchemeEnv&) = delete;
    ~SchemeEnv() = delete;

    static SchemeEnv* create(SchemeMemoryManager *mgr, SchemeEnv *prev, SchemeList *formals, SchemeList *actuals);

    const SchemeRef* lookup(const SchemeSymbol &name) const;
    SchemeRef* lookup(const SchemeSymbol &name) {
        return (SchemeRef*)((const SchemeEnv*)this)->lookup(name);
    }

    void define(SchemeMemoryManager *mgr, const SchemeSymbol &name, const SchemeRef &v);

private:
    enum {
        FIELD_PrevEnv,
        FIELD_Names,
        FIELD_Values,
    };
};

#endif
