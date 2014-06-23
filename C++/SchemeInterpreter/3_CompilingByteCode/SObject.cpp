#include "pch.h"
#include "SObject.h"
#include "STypes.h"

ostream& SObject::writeToStream(ostream &so) const {
    switch (getType()) {
        case SDouble::TYPE:
            return staticCast<SDouble>()->_writeToStream(so);
        case SString::TYPE:
            return staticCast<SString>()->_writeToStream(so);
        case SPair::TYPE:
            return staticCast<SPair>()->_writeToStream(so);
        case SEnv::TYPE:
            return staticCast<SEnv>()->_writeToStream(so);
        case SScriptFunction::TYPE:
            return staticCast<SScriptFunction>()->_writeToStream(so);
        default:
            ASSERT(0);
            return so;
    }
}

bool SObject::equal(const SObject &o) const {
    ASSERT(this != &o);
    ASSERT(getType() == o.getType());

    switch (getType()) {
        case SDouble::TYPE:
            return staticCast<SDouble>()->_equal(*o.staticCast<SDouble>());
        case SString::TYPE:
            return staticCast<SString>()->_equal(*o.staticCast<SString>());
        case SPair::TYPE:
            return staticCast<SPair>()->_equal(*o.staticCast<SPair>());
        case SEnv::TYPE:
            return staticCast<SEnv>()->_equal(*o.staticCast<SEnv>());
        case SScriptFunction::TYPE:
            return staticCast<SScriptFunction>()->_equal(*o.staticCast<SScriptFunction>());
        default:
            ASSERT(0);
            return false;
    }
}

ostream& SExternalObject::writeToStream(ostream &so) const {
    switch (getType()) {
        case SCFunction::TYPE:
            return staticCast<SCFunction>()->_writeToStream(so);
        case SBigInt::TYPE:
            return staticCast<SBigInt>()->_writeToStream(so);
        default:
            ASSERT(0);
            return so;
    }
}

bool SExternalObject::equal(const SExternalObject &o) const {
    ASSERT(this != &o);
    ASSERT(getType() == o.getType());

    switch (getType()) {
        case SCFunction::TYPE:
            return staticCast<SCFunction>()->_equal(*o.staticCast<SCFunction>());
        case SBigInt::TYPE:
            return staticCast<SBigInt>()->_equal(*o.staticCast<SBigInt>());
        default:
            ASSERT(0);
            return false;
    }
}

