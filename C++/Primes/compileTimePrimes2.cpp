#include "pch.h"

//------------------------------
template<bool b, typename T1, typename T2>
struct If {
    typedef T1 Type;
};
template<typename T1, typename T2>
struct If<false, T1, T2> {
    typedef T2 Type;
};

template<int _value, typename _NextT>
struct Node {
    static const int value = _value;
    typedef _NextT Next;
};
struct Nil {
};

template<typename T>
struct Identity {
    typedef T Type;
};

template<int begin, int end> 
struct Range {
    typedef typename If<begin + 1 == end, Identity<Nil>, Range<begin + 1, end>>::Type::Type Next;
    typedef Node<begin, Next> Type;
};

template<template<int> class Pred, typename NodeT>
struct Filter {
    typedef typename Filter<Pred, typename NodeT::Next>::Type Next;
    typedef typename If<Pred<NodeT::value>::value, Node<NodeT::value, Next>, Next>::Type Type;
};
template<template<int> class Pred>
struct Filter<Pred, Nil> {
    typedef Nil Type;
};

template<typename NodeT>
struct Foreach {
    template<typename FuncT>
    static void apply(FuncT f) {
        f(NodeT::value);
        Foreach<typename NodeT::Next>::apply(f);
    }
};
template<>
struct Foreach<Nil> {
    template<typename FuncT>
    static void apply(FuncT f) {
    }
};

//------------------------------
template<typename NodeT>
struct PrimesQueue {
    template<int n>
    struct IsNotDivisibility {
        static const bool value = n % NodeT::value > 0;
    };
    typedef Node<NodeT::value, typename PrimesQueue<typename Filter<IsNotDivisibility, NodeT>::Type>::Type> Type;
};
template<>
struct PrimesQueue<Nil> {
    typedef Nil Type;
};

template<int n>
struct IsEven {
    static const int value = n % 2 == 0;
};

int main() {
    typedef PrimesQueue<Range<2, 64>::Type>::Type Primes;
    Foreach<Primes>::apply([](int i){cout << i << endl;});
}
