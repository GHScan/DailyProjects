#ifndef SCHEME_REF_H
#define SCHEME_REF_H

#include "TaggedPointer.h"

struct SchemeStaticObject;
class SchemeDynamicObject;
class SchemeSymbol;

class SchemeRef: private TaggedPointer {
public:
    static const PtrValue POINTER_MASK = TaggedPointer::POINTER_MASK;
    static const PtrValue TAG_BIT_COUNT = 1;
    static const PtrValue TYPE_BIT_COUNT = 2;
    static const PtrValue TAG_MASK = (1L << TAG_BIT_COUNT) - 1;
    static const PtrValue TYPE_MASK = ((1L << TYPE_BIT_COUNT) - 1) << TAG_BIT_COUNT;
    static_assert(((TYPE_MASK | TAG_MASK) & ~TaggedPointer::TAG_MASK) == 0, "");

public:
    static const PtrValue MAX_INTEGER = TaggedPointer::MAX_LONG;
    static const PtrValue MIN_INTEGER = TaggedPointer::MIN_LONG;

public:
    enum {
        TYPE_Integer = 0 << TAG_BIT_COUNT,
        TYPE_StaticObject = 1 << TAG_BIT_COUNT,
        TYPE_DynamicObject = 2 << TAG_BIT_COUNT,
        TYPE_Symbol = 3 << TAG_BIT_COUNT,
    };

public:
    using TaggedPointer::getPointer;

    SchemeRef() = default;

    explicit SchemeRef(PtrValue n) {
        setInteger(n);
    }

    explicit SchemeRef(SchemeStaticObject *obj) {
        setStaticObject(obj);
    }

    explicit SchemeRef(SchemeDynamicObject *obj) {
        setDynamicObject(obj);
    }

    explicit SchemeRef(const SchemeSymbol *sym) {
        setSymbol(sym);
    }

    int getTag() const {
        return TaggedPointer::get<TAG_MASK>();
    }

    void setTag(PtrValue tag) {
        TaggedPointer::set<TAG_MASK>(tag);
    }

    int getType() const {
        return TaggedPointer::get<TYPE_MASK>();
    }

    PtrValue getInteger() const {
        PtrValue v = get<POINTER_MASK>();

        assert(getType() == TYPE_Integer);
        assert(v >= MIN_INTEGER && v <= MAX_INTEGER);
        return v;
    }

    const SchemeStaticObject* getStaticObject() const {
        const SchemeStaticObject *st = (const SchemeStaticObject*)get<POINTER_MASK>();

        assert(getType() == TYPE_StaticObject);
        assert(st != nullptr);
        return st;
    }
    SchemeStaticObject* getStaticObject() {
        return (SchemeStaticObject*)((const SchemeRef*)this)->getStaticObject();
    }

    const SchemeDynamicObject* getDynamicObject() const {
        const SchemeDynamicObject *dy = (const SchemeDynamicObject*)get<POINTER_MASK>();

        assert(getType() == TYPE_DynamicObject);
        assert(dy != nullptr);
        return dy;
    }
    SchemeDynamicObject* getDynamicObject() {
        return (SchemeDynamicObject*)((const SchemeRef*)this)->getDynamicObject();
    }

    const SchemeSymbol* getSymbol() const {
        const SchemeSymbol* sym = (const SchemeSymbol*)get<POINTER_MASK>();

        assert(getType() == TYPE_Symbol);
        assert(sym != nullptr);
        return sym;
    }
    SchemeSymbol* getSymbol() {
        return (SchemeSymbol*)((const SchemeRef*)this)->getSymbol();
    }

    SchemeRef& setInteger(PtrValue v) {
        TaggedPointer::set<TYPE_MASK | POINTER_MASK >(TYPE_Integer | v);
        return *this;
    }

    SchemeRef& setStaticObject(SchemeStaticObject *st) {
        TaggedPointer::set<TYPE_MASK | POINTER_MASK >(TYPE_StaticObject | (PtrValue)st);
        return *this;
    }

    SchemeRef& setDynamicObject(SchemeDynamicObject *dy) {
        TaggedPointer::set<TYPE_MASK | POINTER_MASK >(TYPE_DynamicObject | (PtrValue)dy);
        return *this;
    }

    SchemeRef& setSymbol(const SchemeSymbol *sym) {
        TaggedPointer::set<TYPE_MASK | POINTER_MASK>(TYPE_Symbol | (PtrValue)sym);
        return *this;
    }

    bool eq(const SchemeRef &o) const {
        return mPointer == o.mPointer;
    }

    bool eqv(const SchemeRef &o) const {
        return equal(o);
    }

    bool equal(const SchemeRef &o) const;
};

#endif
