#include "pch.h"
#include "ScmNumeric.h"
#include "ScmObject.h"
#include "ScmObjectManager.h"
#include "ScmTypes.h"

namespace {

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

}

template<template<typename T> class Op>
static void performArithmetic(ScmObjectManager *mgr, ScmObject **ret, ScmObject *a, ScmObject *b) {
    typedef Op<ScmBigInt::BigInt> BigIntOp;
    typedef Op<double> DoubleOp;

    switch (a->type) {
        case SOT_Int: {
            switch (b->type) {
                case SOT_BigInt:
                    mgr->create<ScmBigInt>(ret, 
                            BigIntOp::invoke(a->staticCast<ScmInt>()->number, b->staticCast<ScmBigInt>()->number));
                    break;
                case SOT_Double:
                    mgr->create<ScmDouble>(ret, 
                            DoubleOp::invoke(a->staticCast<ScmInt>()->number, b->staticCast<ScmDouble>()->number));
                    break;
                default:
                    ASSERT(0);
                    break;
            }
          }
            break;
        case SOT_BigInt: {
            switch (b->type) {
                case SOT_Int:
                    mgr->create<ScmBigInt>(ret, 
                            BigIntOp::invoke(a->staticCast<ScmBigInt>()->number, b->staticCast<ScmInt>()->number));
                    break;
                case SOT_BigInt:
                    mgr->create<ScmBigInt>(ret, 
                            BigIntOp::invoke(a->staticCast<ScmBigInt>()->number, b->staticCast<ScmBigInt>()->number));
                    break;
                case SOT_Double:
                    mgr->create<ScmDouble>(ret, 
                            DoubleOp::invoke(a->staticCast<ScmBigInt>()->toDouble(), b->staticCast<ScmDouble>()->number));
                    break;
                default:
                    ASSERT(0);
                    break;
            }
         }
            break;
        case SOT_Double: {
            switch (b->type) {
                case SOT_Int:
                    mgr->create<ScmDouble>(ret, 
                            DoubleOp::invoke(a->staticCast<ScmDouble>()->number, b->staticCast<ScmInt>()->number));
                    break;
                case SOT_BigInt:
                    mgr->create<ScmDouble>(ret, 
                            DoubleOp::invoke(a->staticCast<ScmDouble>()->number, b->staticCast<ScmBigInt>()->toDouble()));
                    break;
                case SOT_Double:
                    mgr->create<ScmDouble>(ret, 
                            DoubleOp::invoke(a->staticCast<ScmDouble>()->number, b->staticCast<ScmDouble>()->number));
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
void performLogic(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    typedef Op<ScmInt::Int> IntOp;
    typedef Op<ScmBigInt::BigInt> BigIntOp;
    typedef Op<double> DoubleOp;

    ScmObject *a = ret[1], *b = ret[2];
    if (a->type == b->type) { 
        switch (a->type) {
            case SOT_Int:
                mgr->create<ScmInt>(ret, IntOp::invoke(a->staticCast<ScmInt>()->number, b->staticCast<ScmInt>()->number));
                break;
            case SOT_BigInt:
                mgr->create<ScmInt>(ret, BigIntOp::invoke(a->staticCast<ScmBigInt>()->number, b->staticCast<ScmBigInt>()->number));
                break;
            case SOT_Double:
                mgr->create<ScmInt>(ret, DoubleOp::invoke(a->staticCast<ScmDouble>()->number, b->staticCast<ScmDouble>()->number));
                break;
            default:
                ASSERT(0);
                break;
        }
    } else {
        mgr->create<ScmInt>(ret, 0);
    }
}

void ScmNumeric::add(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmObject *a = ret[1], *b = ret[2];
    if (a->type == SOT_Int && b->type == SOT_Int) {
        ScmInt *ai = a->staticCast<ScmInt>(), *bi = b->staticCast<ScmInt>();

        auto r = ai->number + bi->number;
        auto overflow = signBit((ai->number ^ r) & (bi->number ^ r));

        if (overflow) {
            mgr->create<ScmBigInt>(ret, ScmBigInt::BigInt(ai->number) + bi->number);
        } else {
            mgr->create<ScmInt>(ret, r);
        }
        return;
    }

    performArithmetic<Add>(mgr, ret, a, b);
}

void ScmNumeric::sub(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmObject *a = ret[1], *b = ret[2];
    if (a->type == SOT_Int && b->type == SOT_Int) {
        ScmInt *ai = a->staticCast<ScmInt>(), *bi = b->staticCast<ScmInt>();

        auto r = ai->number - bi->number;
        auto overflow = signBit((ai->number ^ bi->number) & (ai->number ^ r));

        if (overflow) {
            mgr->create<ScmBigInt>(ret, ScmBigInt::BigInt(ai->number) - bi->number);
        } else {
            mgr->create<ScmInt>(ret, r);
        }
        return;
    }

    performArithmetic<Sub>(mgr, ret, a, b);
}

void ScmNumeric::mul(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmObject *a = ret[1], *b = ret[2];
    if (a->type == SOT_Int && b->type == SOT_Int) {
        long long na = a->staticCast<ScmInt>()->number, nb = b->staticCast<ScmInt>()->number;

        static_assert(sizeof(ScmInt::Int) < sizeof(long long), "this solution require long long have larger bitwidth");
        long long r = na * nb;

        bool overflow = r < numeric_limits<ScmInt::Int>::min() || r > numeric_limits<ScmInt::Int>::max();

        if (overflow) {
            mgr->create<ScmBigInt>(ret, ScmBigInt::BigInt(na) * nb);
        } else {
            mgr->create<ScmInt>(ret, r);
        }
        return;
    }

    performArithmetic<Mul>(mgr, ret, a, b);
}

void ScmNumeric::div(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmObject *a = ret[1], *b = ret[2];
    if (a->type == SOT_Int && b->type == SOT_Int) {
        ScmInt *ai = a->staticCast<ScmInt>(), *bi = b->staticCast<ScmInt>();

        auto r = ai->number / bi->number;
        auto overflow = 0;

        if (overflow) {
            mgr->create<ScmBigInt>(ret, ScmBigInt::BigInt(ai->number) / bi->number);
        } else {
            mgr->create<ScmInt>(ret, r);
        }
        return;
    }

    performArithmetic<Div>(mgr, ret, a, b);
}

void ScmNumeric::quotient(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    div(mgr, ret, argEnd);

    auto n = ret[0]->dynamicCast<ScmDouble>();
    if (n != nullptr) {
        if (n->number > numeric_limits<ScmInt::Int>::min() && n->number <= numeric_limits<ScmInt::Int>::max()) {
            mgr->create<ScmInt>(ret, (ScmInt::Int)n->number);
        } else {
            mgr->create<ScmBigInt>(ret, n->toBigInt());
        }
    }
}

void ScmNumeric::remainder(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmObject *a = ret[1], *b = ret[2];
    if (a->type == SOT_Int && b->type == SOT_Int) {
        ScmInt *ai = a->staticCast<ScmInt>(), *bi = b->staticCast<ScmInt>();

        auto r = ai->number % bi->number;
        auto overflow = 0;

        if (overflow) {
            mgr->create<ScmBigInt>(ret, ScmBigInt::BigInt(ai->number) % bi->number);
        } else {
            mgr->create<ScmInt>(ret, r);
        }
        return;
    }

    performArithmetic<Mod>(mgr, ret, a, b);
}

void ScmNumeric::equal(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    performLogic<Equal>(mgr, ret, argEnd);
}

void ScmNumeric::less(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    performLogic<Less>(mgr, ret, argEnd);
}

void ScmNumeric::lessEqual(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    performLogic<LessEqual>(mgr, ret, argEnd);
}

void ScmNumeric::greater(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    performLogic<Greater>(mgr, ret, argEnd);
}

void ScmNumeric::greaterEqual(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    performLogic<GreaterEqual>(mgr, ret, argEnd);
}

void ScmNumeric::sqr(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmObject *a = ret[1], *b = ret[1];
    if (a->type == SOT_Int && b->type == SOT_Int) {
        long long na = a->staticCast<ScmInt>()->number, nb = b->staticCast<ScmInt>()->number;

        static_assert(sizeof(ScmInt::Int) < sizeof(long long), "this solution require long long have larger bitwidth");
        long long r = na * nb;

        bool overflow = r < numeric_limits<ScmInt::Int>::min() || r > numeric_limits<ScmInt::Int>::max();

        if (overflow) {
            mgr->create<ScmBigInt>(ret, ScmBigInt::BigInt(na) * nb);
        } else {
            mgr->create<ScmInt>(ret, r);
        }
        return;
    }

    performArithmetic<Mul>(mgr, ret, a, b);
}

void ScmNumeric::sqrt(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    ScmObject *a = ret[1];
    switch (a->type) {
        case SOT_Int:
            mgr->create<ScmDouble>(ret, ::sqrt(a->staticCast<ScmInt>()->number));
            break;
        case SOT_BigInt:
            mgr->create<ScmDouble>(ret, ::sqrt(a->staticCast<ScmBigInt>()->toDouble()));
            break;
        case SOT_Double:
            mgr->create<ScmDouble>(ret, ::sqrt(a->staticCast<ScmDouble>()->number));
            break;
        default:
            ASSERT(0);
            break;
    }
}

void ScmNumeric::_not(ScmObjectManager *mgr, ScmObject **ret, ScmObject **argEnd) {
    mgr->create<ScmInt>(ret, !ret[1]->staticCast<ScmInt>()->number);
}
