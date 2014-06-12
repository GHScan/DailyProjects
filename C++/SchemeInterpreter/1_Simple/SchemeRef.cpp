#include "pch.h"
#include "SchemeRef.h"
#include "SchemeStaticObject.h"
#include "SchemeDynamicObject.h"
#include "SchemeSymbol.h"

bool SchemeRef::equal(const SchemeRef &o) const {
    int type = getType();
    if (type != o.getType()) return false;

    switch (type) {
        case TYPE_Integer:
            return getInteger() == o.getInteger();
        case TYPE_StaticObject:
            return getStaticObject()->equal(*o.getStaticObject());
        case TYPE_DynamicObject:
            return getDynamicObject()->equal(*o.getDynamicObject());
        case TYPE_Symbol:
            return *getSymbol() == *o.getSymbol();
        default:
            assert(0);
            return false;
    }
}

