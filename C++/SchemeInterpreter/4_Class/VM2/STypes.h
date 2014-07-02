#ifndef STYPES_H
#define STYPES_H

#include "SObject.h"
#include "SValue.h"
#include "ScopedValue.h"

struct SFuncProto;
struct SClassProto;

struct SPair: public SObject {
    static const int TYPE = SVT_Pair;

    static int estimateSize(const ScopedValue<SValue> &_car, const ScopedValue<SValue> &_cdr) {
        return toAlignedSize(sizeof(SPair));
    }

    static int estimateSize(const ScopedValue<SValue> &_car) {
        return toAlignedSize(sizeof(SPair));
    }

    bool _equal(const SPair *o) const {
        return car.equal(o->car) && cdr.equal(o->cdr);
    }

    void _writeToStream(ostream &so) const;

    SValue getCar() {
        return car;
    }

    SValue getCdr() {
        return cdr;
    }

    void setCar(SValue v) {
        car = v;
    }

    void setCdr(SValue v) {
        cdr = v;
    }

    SValue car;
    SValue cdr;

private:
    SPair(const ScopedValue<SValue> &_car, const ScopedValue<SValue> &_cdr): SObject(TYPE), car(_car.value), cdr(_cdr.value) {
    }

    SPair(const ScopedValue<SValue> &_car): SObject(TYPE), car(_car.value), cdr(SValue::EMPTY) {
    }

    friend class SObjectManager;
};

struct SEnv: public SObject {
    static const int TYPE = SVT_Env;

    static int estimateSize(const ScopedValue<SObject*> &_prevEnv, int _vCount) {
        return toAlignedSize(sizeof(SEnv) + (_vCount - 1) * sizeof(SValue));
    }

    SEnv *prevEnv;
    int vCount;
    SValue values[1];

    SValue getValue(int i) {
        ASSERT(i >= 0 && i < vCount);
        return values[i];
    }

    void setValue(int i, SValue v) {
        ASSERT(i >= 0 && i < vCount);
        values[i] = v;
    }

    SEnv* getUpEnv(int i) {
        ASSERT(i > 0);

        SEnv *e = prevEnv;
        while (i-- > 1) e = e->prevEnv;
        return e;
    }

    bool _equal(const SEnv *o) const {
        return this == o;
    }

    void _writeToStream(ostream &so) const {
        so << format("{env:%p}", this);
    }

private:
    SEnv(const ScopedValue<SObject*> &_prevEnv, int _vCount): SObject(TYPE), prevEnv(static_cast<SEnv*>(_prevEnv.value)), vCount(_vCount) {
    }

    friend class SObjectManager;
};

struct SFunc: public SObject {
    static const int TYPE = SVT_Func;

    static int estimateSize(const ScopedValue<SObject*> &_env, SFuncProto *_proto) {
        return toAlignedSize(sizeof(SFunc));
    }

    SEnv *env;
    SFuncProto *proto;

    bool _equal(const SFunc *o) const {
        return this == o;
    }

    void _writeToStream(ostream &so) const {
        so << format("{func:%p}", this);
    }

private:
    SFunc(const ScopedValue<SObject*> &_env, SFuncProto *_proto): SObject(TYPE), env(static_cast<SEnv*>(_env.value)), proto(_proto) {
    }

    friend class SObjectManager;
};

struct SNativeFunc: public SObject {
    static const int TYPE = SVT_NativeFunc;

    static int estimateSize(NativeFuncT f) {
        return toAlignedSize(sizeof(SNativeFunc));
    }

    NativeFuncT func;

    bool _equal(const SNativeFunc *o) const {
        return this == o;
    }

    void _writeToStream(ostream &so) const {
        so << format("{nativefunc:%p}", this);
    }

private:
    SNativeFunc(NativeFuncT f): SObject(TYPE), func(f) {
    }

    friend class SObjectManager;
};

struct SClass: public SObject {
    static const int TYPE = SVT_Class;

    static int estimateSize(const ScopedValue<SObject*> &_env, SClassProto *_proto) {
        return toAlignedSize(sizeof(SClass));
    }

    SEnv *env;
    SClassProto *proto;

    SEnv* asEnv() {
        return reinterpret_cast<SEnv*>(this);
    }

    bool _equal(const SClass *o) const {
        return this == o;
    }

    void _writeToStream(ostream &so) const {
        so << format("{class:%p}", this);
    }

private:
    SClass(const ScopedValue<SObject*> &_env, SClassProto *_proto): SObject(TYPE), env(static_cast<SEnv*>(_env.value)), proto(_proto) {
    }

    friend class SObjectManager;
};

#endif
