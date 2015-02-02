#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "Base.h"
#include "List.h"

template<typename Expr, typename Env>
struct _Eval;
template<typename Expr, typename Env>
using Eval = typename _Eval<Expr, Env>::Type;

template<typename List, typename Env>
struct _EvalList {
    typedef Cons<
        Eval<typename List::Car, Env>, 
        typename _EvalList<typename List::Cdr, Env>::Type> Type;
};
template<typename Env>
struct _EvalList<Nil, Env> {
    typedef Nil Type;
};
template<typename List, typename Env>
using EvalList = typename _EvalList<List, Env>::Type;

template<int _value>
struct Var {
    static const int value = _value;
};

template<typename _Formals, typename _Body>
struct Lambda {
    typedef _Formals Formals;
    typedef _Body Body;
};

template<typename Lambda, typename Env>
struct Closure {
    template<typename Actuals>
    struct Apply {
        typedef Eval<
            typename Lambda::Body, 
                     Append<Zip<typename Lambda::Formals, Actuals>, Env>> Type;
    };
};

template<typename Func, typename Actuals>
struct Application { 
};

template<typename Test, typename Conseq, typename Alt>
struct Branch { 
};

template<typename Env>
struct _Eval<True, Env> {
    typedef True Type;
};
template<typename Env>
struct _Eval<False, Env> {
    typedef False Type;
};
template<int n, typename Env>
struct _Eval<Const<n>, Env> {
    typedef Const<n> Type;
};
template<int n, typename Env>
struct _Eval<Var<n>, Env> {
    typedef Lookup<Env, Var<n>> Type;
};
template<typename Formals, typename Body, typename Env>
struct _Eval<Lambda<Formals, Body>, Env> {
    typedef Closure<Lambda<Formals, Body>, Env> Type;
};
template<typename Func, typename Actuals, typename Env>
struct _Eval<Application<Func, Actuals>, Env> {
    typedef typename _Eval<Func, Env>::Type FuncResult;
    typedef EvalList<Actuals, Env> ActualsResult;
    typedef typename FuncResult::template Apply<ActualsResult>::Type Type;
};
template<typename Test, typename Conseq, typename Alt, typename Env>
struct _Eval<Branch<Test, Conseq, Alt>, Env> {
    typedef typename _Eval<Test, Env>::Type TestResult;
    typedef typename If<TestResult, _Eval<Conseq, Env>, _Eval<Alt, Env>>::Type Type;
};

template<template<typename T1, typename ...Tn> class F>
struct WrapFunc {
    template<typename Actuals>
    struct Apply;
    template<typename T1>
    struct Apply<Cons<T1, Nil>> {
        typedef F<T1> Type;
    };
    template<typename T1, typename T2>
    struct Apply<Cons<T1, Cons<T2, Nil>>> {
        typedef F<T1, T2> Type;
    };
};

namespace Implicits {
    template<typename Formals, typename Body>
    using LAM = Lambda<Formals, Body>;

    template<typename Func, typename Actuals>
    using APP = Application<Func, Actuals>;

    typedef Var<0> x;
    typedef Var<1> y;
    typedef Var<2> z;
    typedef Var<3> i;
    typedef Var<4> j;
    typedef Var<5> k;
    typedef Var<6> a;
    typedef Var<7> b;
    typedef Var<8> c;
    typedef Var<9> add;
    typedef Var<10> sub;
    typedef Var<11> mul;
    typedef Var<12> div;
    typedef Var<13> mod;
    typedef Var<14> inc;
    typedef Var<15> sqr;
    typedef Var<16> lt;
    typedef Var<17> leq;
    typedef Var<18> gt;
    typedef Var<19> geq;
    typedef Var<20> eq;
    typedef Var<21> neq;
    typedef Var<22> f;
    typedef Var<23> g;
    typedef Var<24> arg;

    typedef 
        List<
            Cons<add, WrapFunc<Add>>,
            Cons<sub, WrapFunc<Sub>>,
            Cons<mul, WrapFunc<Mul>>,
            Cons<div, WrapFunc<Div>>,
            Cons<mod, WrapFunc<Mod>>,
            Cons<inc, WrapFunc<Inc>>,
            Cons<sqr, WrapFunc<Sqr>>,
            Cons<lt, WrapFunc<Less>>,
            Cons<leq, WrapFunc<LessEq>>,
            Cons<gt, WrapFunc<Greater>>,
            Cons<geq, WrapFunc<GreaterEq>>,
            Cons<eq, WrapFunc<Equal>>,
            Cons<neq, WrapFunc<NotEq>>>
        G;
}

#endif
