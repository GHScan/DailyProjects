#ifndef LIST_H
#define LIST_H

template<bool b, typename T1, typename T2>
struct If {
    typedef T1 type;
};
template<typename T1, typename T2>
struct If<false, T1, T2> {
    typedef T2 type;
};
//------------------------------
template<int _car, typename _cdr>
struct iCons {
    static const int car = _car;
    typedef _cdr cdr;
};

template<typename _car, typename _cdr>
struct tCons {
    typedef _car car;
    typedef _cdr cdr;
};

struct Nil {};

//------------------------------
template<typename ConsT, template<int> class Func>
struct iMap {
    typedef iCons<Func<ConsT::car>::value, typename iMap<typename ConsT::cdr, Func>::type> type;
};
template<template<int> class Func>
struct iMap<Nil, Func> {
    typedef Nil type;
};

template<typename ConsT, template<int> class Pred>
struct iFilter {
    typedef typename iFilter<typename ConsT::cdr, Pred>::type _cdr;
    typedef typename If<Pred<ConsT::car>::value, iCons<ConsT::car, _cdr>, _cdr>::type type;
};
template<template<int> class Pred>
struct iFilter<Nil, Pred> {
    typedef Nil type;
};

template<typename ConsT, typename InitT, template<typename T, int v> class Func>
struct iFoldr {
    typedef typename iFoldr<typename ConsT::cdr, InitT, Func>::type _rest;
    typedef typename Func<_rest, ConsT::car>::type type;
};
template<typename InitT, template<typename T, int v> class Func>
struct iFoldr<Nil, InitT, Func> {
    typedef InitT type;
};

//------------------------------
template<int v, int ...values>
struct iList {
    typedef iCons<v, typename iList<values...>::type> type;
};
template<int v>
struct iList<v> {
    typedef iCons<v, Nil> type;
};

template<typename Cons1T, typename Cons2T>
struct iAppend {
    typedef iCons<Cons1T::car, typename iAppend<typename Cons1T::cdr, Cons2T>::type> type;
};
template<typename Cons2T>
struct iAppend<Nil, Cons2T> {
    typedef Cons2T type;
};

template<typename ConsT>
struct iForeach {
    template<typename FuncT>
    static void apply(FuncT f) {
        f(ConsT::car);
        iForeach<typename ConsT::cdr>::apply(f);
    }
};
template<>
struct iForeach<Nil> {
    template<typename FuncT>
    static void apply(FuncT f) {
    }
};

#endif
