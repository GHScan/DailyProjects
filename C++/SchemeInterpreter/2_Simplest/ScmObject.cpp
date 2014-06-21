#include "pch.h"

#include "ScmObject.h"
#include "ScmTypes.h"
#include "ScmObjectManager.h"

ScmObject* ScmObject::EMPTY = nullptr;

int ScmObject::getHashCode() const {
    switch (type) {
        case ScmSymbol::TYPE:
            return static_cast<const ScmSymbol*>(this)->_getHashCode();
        case ScmInt::TYPE:
            return static_cast<const ScmInt*>(this)->_getHashCode();
        case ScmBigInt::TYPE:
            return static_cast<const ScmBigInt*>(this)->_getHashCode();
        case ScmDouble::TYPE:
            return static_cast<const ScmDouble*>(this)->_getHashCode();
        case ScmString::TYPE:
            return static_cast<const ScmString*>(this)->_getHashCode();
        case ScmPair::TYPE:
            return static_cast<const ScmPair*>(this)->_getHashCode();
        case ScmVector::TYPE:
            return static_cast<const ScmVector*>(this)->_getHashCode();
        case ScmDictionary::TYPE:
            return static_cast<const ScmDictionary*>(this)->_getHashCode();
        case ScmEnv::TYPE:
            return static_cast<const ScmEnv*>(this)->_getHashCode();
        case ScmScriptFunction::TYPE:
            return static_cast<const ScmScriptFunction*>(this)->_getHashCode();
        case ScmCFunction::TYPE:
            return static_cast<const ScmCFunction*>(this)->_getHashCode();
        default:
            ASSERT(0);
            return 0;
    }
}

bool ScmObject::equalDispatch(const ScmObject *o) const {
    switch (type) {
        case ScmSymbol::TYPE:
            return static_cast<const ScmSymbol*>(this)->_equal(static_cast<const ScmSymbol*>(o));
        case ScmInt::TYPE:
            return static_cast<const ScmInt*>(this)->_equal(static_cast<const ScmInt*>(o));
        case ScmBigInt::TYPE:
            return static_cast<const ScmBigInt*>(this)->_equal(static_cast<const ScmBigInt*>(o));
        case ScmDouble::TYPE:
            return static_cast<const ScmDouble*>(this)->_equal(static_cast<const ScmDouble*>(o));
        case ScmString::TYPE:
            return static_cast<const ScmString*>(this)->_equal(static_cast<const ScmString*>(o));
        case ScmPair::TYPE:
            return static_cast<const ScmPair*>(this)->_equal(static_cast<const ScmPair*>(o));
        case ScmVector::TYPE:
            return static_cast<const ScmVector*>(this)->_equal(static_cast<const ScmVector*>(o));
        case ScmDictionary::TYPE:
            return static_cast<const ScmDictionary*>(this)->_equal(static_cast<const ScmDictionary*>(o));
        case ScmEnv::TYPE:
            return static_cast<const ScmEnv*>(this)->_equal(static_cast<const ScmEnv*>(o));
        case ScmScriptFunction::TYPE:
            return static_cast<const ScmScriptFunction*>(this)->_equal(static_cast<const ScmScriptFunction*>(o));
        case ScmCFunction::TYPE:
            return static_cast<const ScmCFunction*>(this)->_equal(static_cast<const ScmCFunction*>(o));
        default:
            ASSERT(0);
            return false;
    }
}

void ScmObject::writeToStream(ostream &so) const {
    switch (type) {
        case ScmSymbol::TYPE:
            static_cast<const ScmSymbol*>(this)->_writeToStream(so);
            break;
        case ScmInt::TYPE:
            static_cast<const ScmInt*>(this)->_writeToStream(so);
            break;
        case ScmBigInt::TYPE:
            static_cast<const ScmBigInt*>(this)->_writeToStream(so);
            break;
        case ScmDouble::TYPE:
            static_cast<const ScmDouble*>(this)->_writeToStream(so);
            break;
        case ScmString::TYPE:
            static_cast<const ScmString*>(this)->_writeToStream(so);
            break;
        case ScmPair::TYPE:
            static_cast<const ScmPair*>(this)->_writeToStream(so);
            break;
        case ScmVector::TYPE:
            static_cast<const ScmVector*>(this)->_writeToStream(so);
            break;
        case ScmDictionary::TYPE:
            static_cast<const ScmDictionary*>(this)->_writeToStream(so);
            break;
        case ScmEnv::TYPE:
            static_cast<const ScmEnv*>(this)->_writeToStream(so);
            break;
        case ScmScriptFunction::TYPE:
            static_cast<const ScmScriptFunction*>(this)->_writeToStream(so);
            break;
        case ScmCFunction::TYPE:
            static_cast<const ScmCFunction*>(this)->_writeToStream(so);
            break;
        default:
            ASSERT(0);
            break;
    }
}
