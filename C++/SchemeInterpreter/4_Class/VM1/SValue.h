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
    static SValue VOID;

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

    SValue() {
        // inorder to declare a var length array, you can do nothing here!
        // mType = SVT_Reserved;
    }

    SValue(double number): mNumber(number), mType(SVT_Number) {
    }

    void setString(Atom *atom) {
        mType = SVT_String;
        mStr = atom;
    }

    void setSymbol(Atom *atom) {
        mType = SVT_Symbol;
        mSymbol = atom;
    }

    SValue(SObject *obj): mObj(obj), mType(obj->getType()) {
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
            case SVT_Pair:
            case SVT_Env:
            case SVT_Func:
            case SVT_NativeFunc:
            case SVT_Class:
                return mObj == o.mObj ? true : mObj->equal(o.mObj);
            default:
                ASSERT(0);
                return false;
        }
    }

    void writeToStream(ostream &so) const {
        switch (mType) {
            case SVT_Reserved:
                if (*this == EMPTY) {
                    so << "()";
                } else if (*this == VOID) {
                } else {
                    ASSERT(0);
                }
                break;
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
            case SVT_Pair:
            case SVT_Env:
            case SVT_Func:
            case SVT_NativeFunc:
            case SVT_Class:
                mObj->writeToStream(so);
                break;
            default:
                ASSERT(0);
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
