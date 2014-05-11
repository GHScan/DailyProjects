#include "pch.h"

#include <functional>

//------------------------------
template<typename RT, typename T, typename ...ArgT>
struct CurriedType {
    typedef function<typename CurriedType<RT, ArgT...>::Type (T)> Type;
};
template<typename RT, typename T>
struct CurriedType<RT, T> {
    typedef function<RT(T)> Type;
};

template<typename RT, typename T>
function<RT(T)> _curry(function<RT(T)> f) {
    return f;
}
template<typename RT, typename T, typename ...ArgT>
auto _curry(function<RT(T, ArgT...)> f) -> typename CurriedType<RT, T, ArgT...>::Type {
    return [f](T &&arg) {
        return _curry(function<RT(ArgT...)>([f, arg](ArgT&& ...args) {
                    return f(arg, forward<ArgT>(args)...);
                }));
    };
}
    
template<typename RT, typename ...ArgT>
auto curry(RT(*f)(ArgT...)) -> decltype(_curry(function<RT(ArgT...)>(f))) {
    return _curry(function<RT(ArgT...)>(f));
}
template<typename RT, typename ...ArgT>
auto curry(function<RT(ArgT...)> f) -> decltype(_curry(f)) {
    return _curry(f);
}

//------------------------------

template<typename T, typename T2>
T sum(T a, T2 b) {
    return a + b;
}
template<typename T, typename ...ArgT>
T sum(T a, ArgT ...b) {
    return a + sum(b...);
}

int main() {
    cout << curry(&sum<int, int>)(1)(2) << endl;
    cout << curry(&sum<int, int, int>)(1)(2)(3) << endl;
    cout << curry(&sum<int, int, int, int>)(1)(2)(3)(4) << endl;
}
