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
inline SValue performArithmetic(SObjectManager *mgr, SValue a, SValue b) {
    typedef Op<SBigInt::BigInt> BigIntOp;
    typedef Op<double> DoubleOp;

    switch (a.getType()) {
        case SVT_Int: {
            switch (b.getType()) {
                case SBigInt::TYPE:
                    return SValue(mgr->createBigInt(BigIntOp::invoke(a.getInt(), b.getExternalObject()->staticCast<SBigInt>()->number)));
                case SDouble::TYPE:
                    return SValue(mgr->createDouble(DoubleOp::invoke(a.getInt(), b.getObject()->staticCast<SDouble>()->number)));
                default:
                    ASSERT(0);
                    return SValue::FALSE;
            }
          }
            break;
        case SBigInt::TYPE: {
            switch (b.getType()) {
                case SVT_Int:
                    return SValue(mgr->createBigInt(BigIntOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->number, b.getInt())));
                case SBigInt::TYPE:
                    return SValue(mgr->createBigInt(BigIntOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->number, b.getExternalObject()->staticCast<SBigInt>()->number)));
                case SDouble::TYPE:
                    return SValue(mgr->createDouble(DoubleOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->toDouble(), b.getObject()->staticCast<SDouble>()->number)));
                default:
                    ASSERT(0);
                    return SValue::FALSE;
            }
         }
            break;
        case SDouble::TYPE: {
            switch (b.getType()) {
                case SVT_Int:
                    return SValue(mgr->createDouble(DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getInt())));
                case SBigInt::TYPE:
                    return SValue(mgr->createDouble(DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getExternalObject()->staticCast<SBigInt>()->toDouble())));
                case SDouble::TYPE:
                    return SValue(mgr->createDouble(DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getObject()->staticCast<SDouble>()->number)));
                default:
                    ASSERT(0);
                    return SValue::FALSE;
            }
         }
            break;
        default:
            ASSERT(0);
            return SValue::FALSE;
    }
}

template<template<typename T> class Op>
inline SValue performLogic(SObjectManager *mgr, SValue a, SValue b) {
    typedef Op<int> IntOp;
    typedef Op<SBigInt::BigInt> BigIntOp;
    typedef Op<double> DoubleOp;

    int atype = a.getType(), btype = b.getType();
    if (atype == btype) { 
        switch (atype) {
            case SVT_Int:
                return IntOp::invoke(a.getInt(), b.getInt()) ? SValue::TRUE : SValue::FALSE;
            case SBigInt::TYPE:
                return BigIntOp::invoke(a.getExternalObject()->staticCast<SBigInt>()->number, b.getExternalObject()->staticCast<SBigInt>()->number) ? SValue::TRUE: SValue::FALSE;
            case SDouble::TYPE:
                return DoubleOp::invoke(a.getObject()->staticCast<SDouble>()->number, b.getObject()->staticCast<SDouble>()->number) ? SValue::TRUE : SValue::FALSE;
            default:
                ASSERT(0);
                return SValue::FALSE;
        }
    } else {
        return SValue::FALSE;
    }
}

}

struct SNumeric {
public:
    static SValue add(SObjectManager *mgr, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            int ai = a.get<SValue::INT_MASK>(), bi = b.get<SValue::INT_MASK>();

            auto r = ai + bi;
            auto overflow = signBit((ai ^ r) & (bi ^ r));

            if (overflow) {
                return SValue(mgr->createBigInt(SBigInt::BigInt(a.getInt()) + b.getInt()));
            } else {
                SValue v;
                v.set<SValue::TAG_MASK>(SValue::TV_Int);
                v.set<SValue::INT_MASK>(r);
                return v;
            }
        }

        return SNumericHelp::performArithmetic<SNumericHelp::Add>(mgr, a, b);
    }

    static SValue sub(SObjectManager *mgr, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            int ai = a.get<SValue::INT_MASK>(), bi = b.get<SValue::INT_MASK>();

            auto r = ai - bi;
            auto overflow = signBit((ai ^ bi) & (ai ^ r));

            if (overflow) {
                return SValue(mgr->createBigInt(SBigInt::BigInt(a.getInt()) + b.getInt()));
            } else {
                SValue v;
                v.set<SValue::TAG_MASK>(SValue::TV_Int);
                v.set<SValue::INT_MASK>(r);
                return v;
            }
        }

        return SNumericHelp::performArithmetic<SNumericHelp::Sub>(mgr, a, b);
    }

    static SValue mul(SObjectManager *mgr, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            long long ai = a.getInt(), bi = b.getInt();

            long long r = ai * bi;
            bool overflow = r < SValue::_INT_MIN || r > SValue::_INT_MAX;

            if (overflow) {
                return SValue(mgr->createBigInt(SBigInt::BigInt(int(ai)) + int(bi)));
            } else {
                return SValue((int)r);
            }
        }

        return SNumericHelp::performArithmetic<SNumericHelp::Mul>(mgr, a, b);
    }

    static SValue div(SObjectManager *mgr, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            long long ai = a.getInt(), bi = b.getInt();

            long long r = ai / bi;
            bool overflow = r < SValue::_INT_MIN || r > SValue::_INT_MAX;

            if (overflow) {
                return SValue(mgr->createBigInt(SBigInt::BigInt(int(ai)) + int(bi)));
            } else {
                return SValue((int)r);
            }
        }

        return SNumericHelp::performArithmetic<SNumericHelp::Div>(mgr, a, b);
    }

    static SValue quotient(SObjectManager *mgr, SValue a, SValue b) {
        auto ret = div(mgr, a, b);

        if (ret.getType() == SDouble::TYPE) {
            auto d = ret.getObject()->staticCast<SDouble>();
            if (d->number < SValue::_INT_MIN || d->number > SValue::_INT_MAX) {
                auto bi(d->toBigInt());
                return SValue(mgr->createBigInt(bi));
            } else {
                return SValue((int)d->number);
            }
        }

        return ret;
    }

    static SValue remainder(SObjectManager *mgr, SValue a, SValue b) {
        if (a.get<SValue::TAG_MASK>() == SValue::TV_Int && b.get<SValue::TAG_MASK>() == SValue::TV_Int) {
            long long ai = a.getInt(), bi = b.getInt();

            long long r = ai % bi;
            bool overflow = r < SValue::_INT_MIN || r > SValue::_INT_MAX;

            if (overflow) {
                return SValue(mgr->createBigInt(SBigInt::BigInt(int(ai)) + int(bi)));
            } else {
                return SValue((int)r);
            }
        }

        return SNumericHelp::performArithmetic<SNumericHelp::Mod>(mgr, a, b);
    }

    static SValue equal(SObjectManager *mgr, SValue a, SValue b) {
        return SNumericHelp::performLogic<SNumericHelp::Equal>(mgr, a, b);
    }

    static SValue less(SObjectManager *mgr, SValue a, SValue b) {
        return SNumericHelp::performLogic<SNumericHelp::Less>(mgr, a, b);
    }

    static SValue lessEqual(SObjectManager *mgr, SValue a, SValue b) {
        return SNumericHelp::performLogic<SNumericHelp::LessEqual>(mgr, a, b);
    }

    static SValue greater(SObjectManager *mgr, SValue a, SValue b) {
        return SNumericHelp::performLogic<SNumericHelp::Greater>(mgr, a, b);
    }

    static SValue greaterEqual(SObjectManager *mgr, SValue a, SValue b) {
        return SNumericHelp::performLogic<SNumericHelp::GreaterEqual>(mgr, a, b);
    }

    static SValue sqr(SObjectManager *mgr, SValue a) {
        return mul(mgr, a, a);
    }

    static SValue sqrt(SObjectManager *mgr, SValue a) {
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
        return SValue(mgr->createDouble(::sqrt(d)));
    }

    static SValue _not(SObjectManager *mgr, SValue a) {
        return a == SValue::TRUE ? SValue::FALSE : SValue::TRUE;
    }

private:
};

#endif
