#ifndef SVALUE_H
#define SVALUE_H

#include "TaggedPointer.h"
#include "SObject.h"

class SSymbol;

enum SValueType {
    SVT_Undefined,
    SVT_Reserved,
    SVT_Bool,
    SVT_Int,
    SVT_Symbol,
    SVT_Double,
    SVT_String,
    SVT_Pair,
    SVT_Env,
    SVT_ScriptFunction,
    SVT_CFunction,
    SVT_BigInt,
};

class SValue: public TaggedPointer {
public:
    static const int INT_BIT_COUNT = sizeof(int) * 8 - TAG_BIT_COUNT;
    static const int _INT_MAX = (1 << (INT_BIT_COUNT - 1)) - 1;
    static const int _INT_MIN = -_INT_MAX - 1;
    static const int INT_MASK = ((1 << INT_BIT_COUNT) - 1) << TAG_BIT_COUNT;

public:
    int getType() const {
        int tag = get<TAG_MASK>();

        ASSERT(tag != TV_Undefined);
        return tag < TV_Object ? tag : (tag == TV_Object ? getObject()->getType() : getExternalObject()->getType());
    }

public:
    const SObject* getObject() const {
        ASSERT(get<TAG_MASK>() == TV_Object);
        return static_cast<const SObject*>(getPointer());
    }

    SObject* getObject() {
        return const_cast<SObject*>(const_cast<const SValue*>(this)->getObject());
    }

    void setObject(SObject *obj) {
        set<TAG_MASK>(TV_Object);
        setPointer(obj);
    }

    const SExternalObject* getExternalObject() const {
        ASSERT(get<TAG_MASK>() == TV_ExternalObject);
        return static_cast<const SExternalObject*>(getPointer());
    }

    SExternalObject* getExternalObject() {
        return const_cast<SExternalObject*>(const_cast<const SValue*>(this)->getExternalObject());
    }

    void setExternalObject(SExternalObject *obj) {
        set<TAG_MASK>(TV_ExternalObject);
        setPointer(obj);
    }

    int getInt() const {
        ASSERT(get<TAG_MASK>() == TV_Int);
        return get<INT_MASK>();
    }

    void setInt(int i) {
        ASSERT(i >= _INT_MIN && i <= _INT_MAX);

        set<TAG_MASK>(TV_Int);
        set<INT_MASK>(i << TAG_BIT_COUNT);
    }

    SSymbol* getSymbol() const {
        ASSERT(get<TAG_MASK>() == TV_Symbol);
        return static_cast<SSymbol*>(getPointer());
    }

    void setSymbol(SSymbol *sym) {
        set<TAG_MASK>(TV_Symbol);
        setPointer(sym);
    }

    bool operator == (const SValue &o) const {
        return mPointer == o.mPointer;
    }

    bool operator != (const SValue &o) const {
        return !(*this == o);
    }

public:
    static SValue TRUE;
    static SValue FALSE;
    static SValue INT_0;
    static SValue INT_1;
    static SValue INT_N_1;
    static SValue EMPTY;
    static SValue VOID;

    SValue() {
        set<TAG_MASK>(TV_Undefined);
    }

    explicit SValue(int i) {
        setInt(i);
    }

    explicit SValue(SSymbol *sym) {
        setSymbol(sym);
    }

private:
    explicit SValue(bool b) {
        set<TAG_MASK>(TV_Bool);
        set<INT_MASK>((b ? 1 : 0) << TAG_BIT_COUNT);
    }

    SValue(int tag, int value) {
        set<TAG_MASK>(tag);
        set<POINTER_MASK>(value << TAG_BIT_COUNT);
    }

private:
    enum TagValue {
        TV_Undefined,
        TV_Reserved,
        TV_Bool,
        TV_Int,
        TV_Symbol,
        TV_Object,
        TV_ExternalObject,
    };
};

typedef function<void(SValue *ret, SValue *arg, SValue *argEnd)> CFunction;

#endif
