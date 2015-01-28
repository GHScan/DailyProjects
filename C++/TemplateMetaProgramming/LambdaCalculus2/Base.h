#ifndef BASE_H
#define BASE_H

struct True {
    static const int value = 1;
};
struct False {
    static const int value = 0;
};

template<typename Test, typename Conseq, typename Alt>
struct _If {
    typedef Conseq Type;
};
template<typename Conseq, typename Alt>
struct _If<False, Conseq, Alt> {
    typedef Alt Type;
};
template<typename Test, typename Conseq, typename Alt>
using If = typename _If<Test, Conseq, Alt>::Type;

template<int n>
struct _ToBoolean {
    typedef True Type;
};
template<>
struct _ToBoolean<0> {
    typedef False Type;
};
template<int n>
using ToBoolean = typename _ToBoolean<n>::Type;

template<int _value>
struct Const {
    static const int value = _value;
};

template<typename T1, typename T2>
using Add = Const<T1::value + T2::value>;
template<typename T1, typename T2>
using Sub = Const<T1::value - T2::value>;
template<typename T1, typename T2>
using Mul = Const<T1::value * T2::value>;
template<typename T1, typename T2>
using Div = Const<T1::value / T2::value>;
template<typename T1, typename T2>
using Mod = Const<T1::value % T2::value>;

template<typename T1, typename T2>
using Less = ToBoolean<T1::value < T2::value>;
template<typename T1, typename T2>
using LessEq = ToBoolean<T1::value <= T2::value>;
template<typename T1, typename T2>
using Greater = ToBoolean<(T1::value > T2::value)>;
template<typename T1, typename T2>
using GreaterEq = ToBoolean<T1::value >= T2::value>;
template<typename T1, typename T2>
using Equal = ToBoolean<T1::value == T2::value>;
template<typename T1, typename T2>
using NotEq = ToBoolean<T1::value != T2::value>;

template<typename T1>
using Inc = Const<T1::value + 1>;
template<typename T1>
using Sqr = Const<T1::value * T1::value>;

template<typename T1>
using Even = ToBoolean<T1::value % 2 == 0>;
template<typename T1>
using Odd = ToBoolean<T1::value % 2 == 1>;

#endif
