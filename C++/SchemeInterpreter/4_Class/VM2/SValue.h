#ifndef SVALUE_H
#define SVALUE_H

#include "SObject.h"
#include "TaggedPointer.h"
#include "Atom.h"

struct SObject;

enum SValueType {
    SVT_Reserved,
    SVT_Bool,
    SVT_Number,
    SVT_String,
    SVT_Symbol,
    SVT_Pair,
    SVT_Env,
    SVT_Func,
    SVT_NativeFunc,
    SVT_Class,
};

struct SValue: private TaggedPointer {
    static SValue TRUE;
    static SValue FALSE;
    static SValue EMPTY;
    static SValue VOID;

    int getType() const {
        int tt = get<TAG_MASK>();
        return tt == TT_Object ? getObject()->getType() : tt;
    }

    bool isObject() const {
        return get<TAG_MASK>() == TT_Object;
    }

    SObject* getObject() {
        ASSERT(isObject());
        return static_cast<SObject*>(getPointer());
    }

    const SObject* getObject() const {
        ASSERT(isObject());
        return static_cast<const SObject*>(getPointer());
    }

    template<typename T>
    T* getObject() {
        auto obj = getObject();
        ASSERT(obj->getType() == T::TYPE);
        return static_cast<T*>(obj);
    }

    template<typename T>
    const T* getObject() const {
        auto obj = getObject();
        ASSERT(obj->getType() == T::TYPE);
        return static_cast<const T*>(obj);
    }

    PtrValue getNumber() const {
        ASSERT(get<TAG_MASK>() == TT_Number);
        return get<POINTER_MASK>() >> TAG_BIT_COUNT;
    }

    Atom* getString() const {
        ASSERT(get<TAG_MASK>() == TT_String);
        return static_cast<Atom*>(getPointer());
    }

    Atom* getSymbol() const {
        ASSERT(get<TAG_MASK>() == TT_Symbol);
        return static_cast<Atom*>(getPointer());
    }

    SValue() {
        // inorder to declare a var length array, you can do nothing here!
        // mType = SVT_Reserved;
    }

    explicit SValue(PtrValue number) {
        set<TAG_MASK>(TT_Number);
        set<POINTER_MASK>(number << TAG_BIT_COUNT);
    }

    void setString(Atom *atom) {
        set<TAG_MASK>(TT_String);
        setPointer(atom);
    }

    void setSymbol(Atom *atom) {
        set<TAG_MASK>(TT_Symbol);
        setPointer(atom);
    }

    explicit SValue(SObject *obj) {
        set<TAG_MASK>(TT_Object);
        setPointer(obj);
    }

    bool operator == (SValue o) const {
        return mPointer == o.mPointer;
    }

    bool operator != (SValue o) const {
        return mPointer != o.mPointer;
    }

    bool equal(SValue o) const {
        int tt = get<TAG_MASK>();
        if (tt != o.get<TAG_MASK>()) return false;

        switch (tt) {
            case TT_Reserved:
            case TT_Bool:
            case TT_Number:
            case TT_String:
            case TT_Symbol:
                return getPointer() == o.getPointer();
            case TT_Object: {
                    const SObject *a = getObject(), *b = o.getObject();
                    return a == b || a->equal(b);
                }
            default:
                ASSERT(0);
                return false;
        }
    }

    void writeToStream(ostream &so) const {
        switch (get<TAG_MASK>()) {
            case TT_Reserved:
                if (*this == EMPTY) {
                    so << "()";
                } else if (*this == VOID) {
                } else {
                    ASSERT(0);
                }
                break;
            case TT_Bool:
                so << (*this == SValue::TRUE ? "true" : "false");
                break;
            case TT_Number:
                so << getNumber();
                break;
            case TT_String:
                so << '"' << escapeString(getString()->c_str(), "\"") << '"';
                break;
            case TT_Symbol:
                so << getSymbol()->c_str();
                break;
            case TT_Object:
                getObject()->writeToStream(so);
                break;
            default:
                ASSERT(0);
                break;
        }
    }

private:
    SValue(PtrValue tag, PtrValue value) {
        set<TAG_MASK>(tag);
        set<POINTER_MASK>(value << TAG_BIT_COUNT);
    }

private:
    enum TagType {
        TT_Reserved,
        TT_Bool,
        TT_Number,
        TT_String,
        TT_Symbol,
        TT_Object,
    };
};

typedef function<void(class SObjectManager *mgr, SValue *ret)> NativeFuncT;

#endif
