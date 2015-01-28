#ifndef LIST_H
#define LIST_H

#include "Base.h"

struct Nil {};
template<typename _Car, typename _Cdr>
struct Cons {
    typedef _Car Car;
    typedef _Cdr Cdr;
};

template<typename List>
struct _Length {
    typedef Add<Const<1>, typename _Length<typename List::Cdr>::Type> Type;
};
template<>
struct _Length<Nil> {
    typedef Const<0> Type;
};
template<typename List>
using Length = typename _Length<List>::Type;

template<typename T1, typename ...Tn>
struct _List {
    typedef Cons<T1, typename _List<Tn...>::Type> Type;
};
template<typename T1>
struct _List<T1> {
    typedef Cons<T1, Nil> Type;
};
template<typename ...Tn>
using List = typename _List<Tn...>::Type;

template<int n, int ...ns>
struct _IntList {
    typedef Cons<Const<n>, typename _IntList<ns...>::Type> Type;
};
template<int n>
struct _IntList<n> {
    typedef Cons<Const<n>, Nil> Type;
};
template<int ...ns>
using IntList = typename _IntList<ns...>::Type;

template<int first, int last>
struct _Range {
    typedef Cons<Const<first>, typename _Range<first + 1, last>::Type> Type;
};
template<int last>
struct _Range<last, last> {
    typedef Cons<Const<last>, Nil> Type;
};
template<int first, int last>
using Range = typename _Range<first, last>::Type;

template<typename List1, typename List2>
struct _Append {
    typedef Cons<
        typename List1::Car, 
                 typename _Append<typename List1::Cdr, List2>::Type> Type;
};
template<typename List2>
struct _Append<Nil, List2> {
    typedef List2 Type;
};
template<typename List1, typename List2>
using Append = typename _Append<List1, List2>::Type;

template<typename List1, typename List2>
struct _Zip {
    typedef Cons<
        Cons<typename List1::Car, typename List2::Car>, 
        typename _Zip<typename List1::Cdr, typename List2::Cdr>::Type> Type;
};
template<typename List1>
struct _Zip<List1, Nil> {
    typedef Nil Type;
};
template<typename List2>
struct _Zip<Nil, List2> {
    typedef Nil Type;
};
template<>
struct _Zip<Nil, Nil> {
    typedef Nil Type;
};
template<typename List1, typename List2>
using Zip = typename _Zip<List1, List2>::Type;

template<typename List, template<typename T> class F>
struct _Map {
    typedef Cons<
        F<typename List::Car>, 
        typename _Map<typename List::Cdr, F>::Type> Type;
};
template<template<typename T> class F>
struct _Map<Nil, F> {
    typedef Nil Type;
};
template<typename List, template<typename T> class F>
using Map = typename _Map<List, F>::Type;

template<typename List, template<typename T> class F>
struct _Filter {
    typedef typename _Filter<typename List::Cdr, F>::Type _Cdr;
    typedef If<
        F<typename List::Car>,
        Cons<typename List::Car, _Cdr>,
        _Cdr> Type;
};
template<template<typename T> class F>
struct _Filter<Nil, F> {
    typedef Nil Type;
};
template<typename List, template<typename T> class F>
using Filter = typename _Filter<List, F>::Type;

template<typename List, typename Init, template<typename T, typename T2> class F>
struct _Reduce {
    typedef typename _Reduce<
        typename List::Cdr, 
                 F<Init, typename List::Car>, 
                 F>::Type Type;
};
template<typename Init, template<typename T, typename T2> class F>
struct _Reduce<Nil, Init, F> {
    typedef Init Type;
};
template<typename List, typename Init, template<typename T, typename T2> class F>
using Reduce = typename _Reduce<List, Init, F>::Type;

template<typename List, typename Key>
struct _Assq {
    typedef If<
        Equal<typename List::Car::Car, Key>,
        typename List::Car,
        typename _Assq<typename List::Cdr, Key>::Type> Type;
};
template<typename Key>
struct _Assq<Nil, Key> {
    typedef False Type;
};
template<typename List, typename Key>
using Assq = typename _Assq<List, Key>::Type;

template<typename List, typename Key>
using Lookup = typename Assq<List, Key>::Cdr;

template<typename List>
using Sum = Reduce<List, Const<0>, Add>;

template<typename List>
struct Foreach {
    template<typename F>
    static void apply(F f) {
        f(List::Car::value);
        Foreach<typename List::Cdr>::apply(f);
    }
};
template<>
struct Foreach<Nil> {
    template<typename F>
    static void apply(F f) {
    }
};

#endif
