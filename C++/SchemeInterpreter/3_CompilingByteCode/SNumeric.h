#ifndef SNUMERIC_H
#define SNUMERIC_H

#include "SValue.h"
#include "SObjectManager.h"

namespace SNumericHelp {

template<typename T>
struct Add {
    static T invoke(const T& a, const T &b) { return a + b; }
};
template<typename T>
struct Sub {
    static T invoke(const T& a, const T &b) { return a - b; }
};
template<typename T>
struct Mul {
    static T invoke(const T& a, const T &b) { return a * b; }
};
template<typename T>
struct Div {
    static T invoke(const T& a, const T &b) { return a / b; }
};
template<typename T>
struct Mod {
    static T invoke(const T& a, const T &b) { return a % b; }
};
template<>
struct Mod<double> {
    static double invoke(double a, double b) { return fmod(a, b); }
};

template<typename T>
struct Equal {
    static int invoke(const T& a, const T &b) { return a == b; }
};
template<typename T>
struct Less {
    static int invoke(const T& a, const T &b) { return a < b; }
};
template<typename T>
struct LessEqual {
    static int invoke(const T& a, const T &b) { return a <= b; }
};
template<typename T>
struct Greater {
    static int invoke(const T& a, const T &b) { return a > b; }
};
template<typename T>
struct GreaterEqual {
    static int invoke(const T& a, const T &b) { return a >= b; }
};

template<template<typename T> class Op>
inline void performArithmetic(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
    typedef Op<SBigInt::BigInt> BigIntOp;
    typedef Op<double> DoubleOp;

    switch (a.getType()) {
        case SVT_Int: {
            switch (b.getType()) {
                case SBigInt::TYPE:
                    mgr->createBigInt(ret, 
                            BigIntOp::invoke(a.getInt(), b.getExternalObject()->staticCast<SBigInt>()->number));
                    break;
                case SDouble::TYPE:
                    mgr->createDouble(ret, 
                            DoubleOp::invoke(a.getInt(), b.getObject()->staticCast<SDouble>()->number));
                    break;
                default:
                    ASSERT(0);
                    break;
            }
          }
            break;
        case SBigInt::TYPE: {
            switch (b.getType()) {
                case SVT_Int:
                    mgr->createBigInt(ret, 
                            BigIntOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->number, b.getInt()));
                    break;
                case SBigInt::TYPE:
                    mgr->createBigInt(ret, 
                            BigIntOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->number, b.getExternalObject()->staticCast<SBigInt>()->number));
                    break;
                case SDouble::TYPE:
                    mgr->createDouble(ret, 
                            DoubleOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->toDouble(), b.getObject()->staticCast<SDouble>()->number));
                    break;
                default:
                    ASSERT(0);
                    break;
            }
         }
            break;
        case SDouble::TYPE: {
            switch (b.getType()) {
                case SVT_Int:
                    mgr->createDouble(ret, 
                            DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getInt()));
                    break;
                case SBigInt::TYPE:
                    mgr->createDouble(ret, 
                            DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getExternalObject()->staticCast<SBigInt>()->toDouble()));
                    break;
                case SDouble::TYPE:
                    mgr->createDouble(ret, 
                            DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getObject()->staticCast<SDouble>()->number));
                    break;
                default:
                    ASSERT(0);
                    break;
            }
         }
            break;
        default:
            ASSERT(0);
            break;
    }
}

template<template<typename T> class Op>
inline void performLogic(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
    typedef Op<int> IntOp;
    typedef Op<SBigInt::BigInt> BigIntOp;
    typedef Op<double> DoubleOp;

    int atype = a.getType(), btype = b.getType();
    if (atype == btype) { 
        switch (atype) {
            case SVT_Int:
                mgr->createBool(ret, IntOp::invoke(a.getInt(), b.getInt()));
                break;
            case SBigInt::TYPE:
                mgr->createBool(ret, BigIntOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->number, b.getExternalObject()->staticCast<SBigInt>()->number));
                break;
            case SDouble::TYPE:
                mgr->createBool(ret, DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getObject()->staticCast<SDouble>()->number));
                break;
            default:
                ASSERT(0);
                break;
        }
    } else {
        mgr->createBool(ret, false);
    }
}

}

struct SNumeric {
public:
    static void add(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            int ai = a.get<SValue::INT_MASK>(), bi = b.get<SValue::INT_MASK>();

            auto r = ai + bi;
            auto overflow = signBit((ai ^ r) & (bi ^ r));

            if (overflow) {
                mgr->createBigInt(ret, SBigInt::BigInt(a.getInt()) + b.getInt());
            } else {
                ret->set<SValue::TAG_MASK>(SValue::TV_Int);
                ret->set<SValue::INT_MASK>(r);
            }

            return;
        }

        SNumericHelp::performArithmetic<SNumericHelp::Add>(mgr, ret, a, b);
    }

    static void sub(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            int ai = a.get<SValue::INT_MASK>(), bi = b.get<SValue::INT_MASK>();

            auto r = ai - bi;
            auto overflow = signBit((ai ^ bi) & (ai ^ r));

            if (overflow) {
                mgr->createBigInt(ret, SBigInt::BigInt(a.getInt()) + b.getInt());
            } else {
                ret->set<SValue::TAG_MASK>(SValue::TV_Int);
                ret->set<SValue::INT_MASK>(r);
            }

            return;
        }

        SNumericHelp::performArithmetic<SNumericHelp::Sub>(mgr, ret, a, b);
    }

    static void mul(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            long long ai = a.getInt(), bi = b.getInt();

            long long r = ai * bi;
            bool overflow = r < SValue::_INT_MIN || r > SValue::_INT_MAX;

            if (overflow) {
                mgr->createBigInt(ret, SBigInt::BigInt(ai) + bi);
            } else {
                mgr->createInt(ret, (int)r);
            }

            return;
        }

        SNumericHelp::performArithmetic<SNumericHelp::Mul>(mgr, ret, a, b);
    }

    static void div(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            long long ai = a.getInt(), bi = b.getInt();

            long long r = ai / bi;
            bool overflow = r < SValue::_INT_MIN || r > SValue::_INT_MAX;

            if (overflow) {
                mgr->createBigInt(ret, SBigInt::BigInt(ai) + bi);
            } else {
                mgr->createInt(ret, (int)r);
            }

            return;
        }

        SNumericHelp::performArithmetic<SNumericHelp::Div>(mgr, ret, a, b);
    }

    static void quotient(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        div(mgr, ret, a, b);

        if (ret->getType() == SDouble::TYPE) {
            auto d = ret->getObject()->staticCast<SDouble>();
            if (d->number < SValue::_INT_MIN || d->number > SValue::_INT_MAX) {
                auto bi(d->toBigInt());
                mgr->createBigInt(ret, bi);
            } else {
                mgr->createInt(ret, (int)d->number);
            }
        }
    }

    static void remainder(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            long long ai = a.getInt(), bi = b.getInt();

            long long r = ai % bi;
            bool overflow = r < SValue::_INT_MIN || r > SValue::_INT_MAX;

            if (overflow) {
                mgr->createBigInt(ret, SBigInt::BigInt(ai) + bi);
            } else {
                mgr->createInt(ret, (int)r);
            }

            return;
        }

        SNumericHelp::performArithmetic<SNumericHelp::Mod>(mgr, ret, a, b);
    }

    static void equal(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        SNumericHelp::performLogic<SNumericHelp::Equal>(mgr, ret, a, b);
    }

    static void less(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        SNumericHelp::performLogic<SNumericHelp::Less>(mgr, ret, a, b);
    }

    static void lessEqual(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        SNumericHelp::performLogic<SNumericHelp::LessEqual>(mgr, ret, a, b);
    }

    static void greater(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        SNumericHelp::performLogic<SNumericHelp::Greater>(mgr, ret, a, b);
    }

    static void greaterEqual(SObjectManager *mgr, SValue *ret, SValue a, SValue b) {
        SNumericHelp::performLogic<SNumericHelp::GreaterEqual>(mgr, ret, a, b);
    }

    static void sqr(SObjectManager *mgr, SValue *ret, SValue a) {
        mul(mgr, ret, a, a);
    }

    static void sqrt(SObjectManager *mgr, SValue *ret, SValue a) {
        double d;
        switch (a.getType()) {
            case SVT_Int:
                d = a.getInt();
                break;
            case SDouble::TYPE:
                d = a.getObject()->staticCast<SDouble>()->number;
                break;
            case SBigInt::TYPE:
                d = a.getExternalObject()->staticCast<SBigInt>()->toDouble();
                break;
            default:
                ASSERT(0);
                break;
        }
        mgr->createDouble(ret, ::sqrt(d));
    }

    static void _not(SObjectManager *mgr, SValue *ret, SValue a) {
        mgr->createBool(ret, a == SValue::FALSE);
    }

private:
};

#endif
