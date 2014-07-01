#include "pch.h"
#include "SObject.h"
#include "STypes.h"

vector<SObject*> SObject::sYoungContainer;

bool SObject::equal(const SObject *o) const {
    switch (mType) {
        case SVT_Pair:
            return static_cast<const SPair*>(this)->_equal(static_cast<const SPair*>(o));
        case SVT_Env:
            return static_cast<const SEnv*>(this)->_equal(static_cast<const SEnv*>(o));
        case SVT_Func:
            return static_cast<const SFunc*>(this)->_equal(static_cast<const SFunc*>(o));
        case SVT_NativeFunc:
            return static_cast<const SNativeFunc*>(this)->_equal(static_cast<const SNativeFunc*>(o));
        case SVT_Class:
            return static_cast<const SClass*>(this)->_equal(static_cast<const SClass*>(o));
        default:
            ASSERT(0);
            return false;
    }
}

void SObject::writeToStream(ostream &so) const {
    switch (mType) {
        case SVT_Pair:
            static_cast<const SPair*>(this)->_writeToStream(so);
            break;
        case SVT_Env:
            static_cast<const SEnv*>(this)->_writeToStream(so);
            break;
        case SVT_Func:
            static_cast<const SFunc*>(this)->_writeToStream(so);
            break;
        case SVT_NativeFunc:
            static_cast<const SNativeFunc*>(this)->_writeToStream(so);
            break;
        case SVT_Class:
            static_cast<const SClass*>(this)->_writeToStream(so);
            break;
        default:
            ASSERT(0);
            break;
    }
}
