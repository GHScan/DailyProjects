#include "pch.h"
#include "SchemeStaticObject.h"
#include "SchemeMemoryManager.h"

SchemePair* SchemePair::create(SchemeMemoryManager *mgr) {
    return new (mgr->mallocSchemeRefs(2)) SchemePair();
}

SchemeVector* SchemeVector::create(SchemeMemoryManager *mgr, int size) {
    return new (mgr->mallocSchemeRefs(size + 1)) SchemeVector(size);
}
