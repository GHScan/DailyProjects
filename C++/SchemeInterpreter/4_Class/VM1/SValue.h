#ifndef SVALUE_H
#define SVALUE_H

#include "SObject.h"
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

struct SValue {
    static SValue TRUE;
    static SValue FALSE;
    static SValue EMPTY;

    int getType() const {
        return mType;
    }

    double getNumber() {
        ASSERT(mType == SVT_Number);
        return mNumber;
    }

    Atom* getString() {
        ASSERT(mType == SVT_String);
        return mStr;
    }

    Atom* getSymbol() {
        ASSERT(mType == SVT_Symbol);
        return mSymbol;
    }

    template<typename T>
    T* getObject() {
        ASSERT(mType == T::TYPE && mObj->getType() == T::TYPE);
        return static_cast<T*>(mObj);
    }

    template<typename T>
    const T* getObject() const {
        ASSERT(mType == T::TYPE && mObj->getType() == T::TYPE);
        return static_cast<const T*>(mObj);
    }

    SValue(): mType(SVT_Reserved) {}

    void setNumber(double number) {
        mType = SVT_Number;
        mNumber = number;
    }

    void setString(Atom *atom) {
        mType = SVT_String;
        mStr = atom;
    }

    void setSymbol(Atom *atom) {
        mType = SVT_Symbol;
        mSymbol = atom;
    }

    void setObject(SObject *obj) {
        mType = obj->getType();
        mObj = obj;
    }

    bool operator == (SValue o) const {
        return mType == o.mType && mNumber == o.mNumber;
    }

    bool operator != (SValue o) const {
        return !(*this == o);
    }

    bool equal(SValue o) const {
        if (mType != o.mType) return false;
        switch (mType) {
            case SVT_Reserved:
                return mNumber == o.mNumber;
            case SVT_Bool:
                return mBool == o.mBool;
            case SVT_Number:
                return mNumber == o.mNumber;
            case SVT_String:
                return mStr == o.mStr;
            case SVT_Symbol:
                return mSymbol == o.mSymbol;
            default:
                return mObj == o.mObj ? true : mObj->equal(o.mObj);
        }
    }

    void writeToStream(ostream &so) const {
        if (*this == EMPTY) {
            so << "()";
            return;
        }

        switch (mType) {
            case SVT_Bool:
                so << (mBool ? "true" : "false");
                break;
            case SVT_Number:
                so << mNumber;
                break;
            case SVT_String:
                so << '"' << escapeString(mStr->c_str(), "\"") << '"';
                break;
            case SVT_Symbol:
                so << mSymbol->c_str();
                break;
            default:
                mObj->writeToStream(so);
                break;
        }
    }

private:
    SValue(bool _b): mBool(_b), mType(SVT_Bool) {
    }

    static SValue createReserved(int v) {
        SValue r;
        r.mType = SVT_Reserved;
        r.mNumber = v;
        return r;
    }

private:
    union {
        bool mBool;
        double mNumber;
        Atom* mStr;
        Atom* mSymbol;
        SObject* mObj;
    };
    int mType;
};

typedef function<void(class SObjectManager *mgr, SValue *ret)> NativeFuncT;

#endif
