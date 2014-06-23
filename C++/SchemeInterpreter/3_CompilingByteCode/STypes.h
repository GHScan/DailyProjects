#ifndef STYPES_H
#define STYPES_H

#include <integer.h>

#include "SObject.h"
#include "SValue.h"
#include "SScriptFunctionProto.h"

class SObjectManager;

struct SDouble: public SObject {
    static const int TYPE = SVT_Double;
    static int estimateAlignedSize(double n) {
        static_assert(ALIGNMENT >= alignof(SDouble), "");

        return objectSizeToAlignedSize(sizeof(SDouble));
    }

    double number;

    bool _equal(const SDouble &o) const {
        return fabs(number - o.number) < 0.0001;
    }

    ostream& _writeToStream(ostream& so) const {
        return so << number;
    }

private:
    friend class SObjectManager;

    explicit SDouble(double n): 
        SObject(TYPE, estimateAlignedSize(n)), number(n) {
    }
};

struct SString: public SObject {
    static const int TYPE = SVT_String;
    static int estimateAlignedSize(const char *str) {
        static_assert(ALIGNMENT >= alignof(SString), "");

        return objectSizeToAlignedSize(sizeof(SString) + strlen(str));
    }

    char str[1];

    bool _equal(const SString &o) const {
        return strcmp(str, o.str) == 0;
    }

    ostream& _writeToStream(ostream& so) const {
        return so << '"' << escapeString(str, "\"") << '"';
    }

private:
    friend class SObjectManager;

    explicit SString(const char *_str): 
        SObject(TYPE, estimateAlignedSize(_str)) {
        strcpy(str, _str);
    }
};

struct SPair: public SObject {
    static const int TYPE = SVT_Pair;
    static int estimateAlignedSize(SValue car, SValue cdr) {
        ASSERT(ALIGNMENT >= alignof(SPair));

        return objectSizeToAlignedSize(sizeof(SPair));
    }

    SValue car;
    SValue cdr;

    bool _equal(const SPair &o) const {
        return car.equal(o.car) && cdr.equal(o.cdr);
    }

    ostream& _writeToStream(ostream& so) const;

private:
    friend class SObjectManager;

    explicit SPair(SValue _car, SValue _cdr): 
        SObject(TYPE, estimateAlignedSize(_car, _cdr)), car(_car), cdr(_cdr) {
    }
};

struct SEnv: public SObject {
    static const int TYPE = SVT_Env;
    static int estimateAlignedSize(SEnv *prevEnv, int localCount) {
        ASSERT(ALIGNMENT >= alignof(SEnv));

        return objectSizeToAlignedSize(sizeof(SEnv) + (localCount - 1) * sizeof(SValue));
    }

    SEnv *prevEnv;
    int localCount;
    SValue locals[1];

    SValue getLocal(int index) const {
        ASSERT(index >= 0 && index < localCount);

        return locals[index];
    }

    void setLocal(int index, SValue v) {
        ASSERT(index >= 0 && index < localCount);

        locals[index] = v;
    }

    bool _equal(const SEnv &o) const {
        return this == &o;
    }

    ostream& _writeToStream(ostream& so) const {
        return so << format("{env:%p}", this);
    }

private:
    friend class SObjectManager;

    explicit SEnv(SEnv *_prevEnv, int _localCount): 
        SObject(TYPE, estimateAlignedSize(_prevEnv, localCount)), prevEnv(_prevEnv), localCount(_localCount) {
    }
};

struct SScriptFunction: public SObject {
    static const int TYPE = SVT_ScriptFunction;
    static int estimateAlignedSize(SScriptFunctionProto *proto, SEnv *env) {
        ASSERT(ALIGNMENT >= alignof(SScriptFunction));

        return objectSizeToAlignedSize(sizeof(SScriptFunction) + ((int)proto->freeAddresses.size() - 1) * sizeof(freeVars[0]));
    }

    SScriptFunctionProto *proto;
    SEnv *env;
    SValue *freeVars[1];

    SValue getFree(int freeIndex) const {
        ASSERT(freeIndex >= 0 && freeIndex < (int)proto->freeAddresses.size());

        return *freeVars[freeIndex];
    }

    void setFree(int freeIndex, SValue v) {
        ASSERT(freeIndex >= 0 && freeIndex < (int)proto->freeAddresses.size());

        *freeVars[freeIndex] = v;
    }

    bool _equal(const SScriptFunction &o) const {
        return this == &o;
    }

    ostream& _writeToStream(ostream& so) const {
        return so << format("{scriptFunction:%p}", this);
    }

private:
    friend class SObjectManager;

    explicit SScriptFunction(SScriptFunctionProto *_proto, SEnv *_env): 
        SObject(TYPE, estimateAlignedSize(_proto, _env)), proto(_proto), env(_env) {
        
        int i = 0;
        for (auto address : _proto->freeAddresses) {
            int envIndex = address.getEnvIndex();
            int index = address.getVarIndex();

            SEnv *env = env;
            for (int i = 1; i < envIndex; ++i) env = env->prevEnv;
            ASSERT(index >= 0 && index < env->localCount);

            freeVars[i] = &env->locals[index];
        }
    }
};

struct SCFunction: public SExternalObject {
    static const int TYPE = SVT_CFunction;

    CFunction func;

    bool _equal(const SCFunction& o) const {
        return this == &o;
    }

    ostream& _writeToStream(ostream& so) const {
        return so << format("{cFunction:%p}", this);
    }

private:
    friend class SObjectManager;

    explicit SCFunction(CFunction f): 
        SExternalObject(TYPE), func(f) {
    }
};

struct SBigInt: public SExternalObject {
    static const int TYPE = SVT_BigInt;

    typedef CryptoPP::Integer BigInt;
    typedef CryptoPP::word Word;

    BigInt number;

    bool _equal(const SBigInt& o) const {
        return number == o.number;
    }

    ostream& _writeToStream(ostream& so) const {
        return so << number;
    }

private:
    friend class SObjectManager;

    explicit SBigInt(const BigInt &n): 
        SExternalObject(TYPE), number(n) {
    }
};

#endif
