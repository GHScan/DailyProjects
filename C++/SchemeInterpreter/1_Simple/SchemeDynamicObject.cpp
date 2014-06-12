#include "pch.h"
#include "SchemeDynamicObject.h"
#include "SchemeMemoryManager.h"

SchemeFloat* SchemeFloat::create(SchemeMemoryManager *mgr, double n) {
    SchemeFloat *p = new SchemeFloat(n);
    mgr->addDynamicObject(p);
    return p;
}

SchemeString* SchemeString::create(SchemeMemoryManager *mgr, const char *s) {
    SchemeString *p = new SchemeString(s);
    mgr->addDynamicObject(p);
    return p;
}

SchemeBigInteger* SchemeBigInteger::create(SchemeMemoryManager *mgr, const BigInteger &n) {
    SchemeBigInteger *p = new SchemeBigInteger(n);
    mgr->addDynamicObject(p);
    return p;
}
