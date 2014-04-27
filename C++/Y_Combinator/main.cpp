#include "pch.h" 

#include <functional>

//////////////////////////////
// why need Y-combinator ?
// if your procedure is stateless, you can use global function or other
// compile-time scope like class scope, but once your procedure have state,
// then you will face the lambda recursion problem. Y-combinator is a
// solution
// because the upvalue in cpp has lifetime issues, lambda could not
// reference themselves:
static function<int(int)> issueCase_fib() {
    function<int(int)> fib = [&fib](int x) {
        if (x <= 1) return 1;
        return fib(x - 1) + fib(x - 2);
    };

    // it works
    printf("issueCase_fib: %d\n", fib(7));

    // but after returning, the "[&fib]" will be illegal!
    return fib;
}

//////////////////////////////
// simplest solution is function object
// but you could not use lambda, it's hard to use
struct OB_Fib {
    int operator () (int x) const {
        if (x <= 1) return 1;
        return (*this)(x - 1) + (*this)(x - 2);
    }
};

static function<int(int)> correctCase_objectBased() {
    OB_Fib fib;
    // it works
    printf("correctCase_objectBased: %d\n", fib(7));
    // also correct
    return fib;
}

//////////////////////////////
// the Y-combinator solution
// use a adapter to wrap the lambda to function object
//
template<typename RT, typename ...ArgT>
class Y_Adapter {
public:
    typedef function<RT(function<RT(ArgT...)>, ArgT...)> OriginFuncT;
    Y_Adapter(OriginFuncT f): mF(f){}
    RT operator () (ArgT&& ...arg) const {
        return mF(*this, forward<ArgT>(arg)...);
    }
private:
    OriginFuncT mF;
};
template<typename RT, typename ...ArgT>
function<RT(ArgT...)> Y(function<RT(function<RT(ArgT...)>, ArgT...)> f) {
    return Y_Adapter<RT, ArgT...>(f);
}

static function<int(int)> correctCase_Y() {
    auto fib = Y<int, int>([](function<int(int)> self, int x){
            if (x <= 1) return 1;
            else return self(x - 1) + self(x - 2);
            });
    printf("correctCase_Y: %d\n", fib(7));
    return fib;
}
//////////////////////////////
// another Y-combinator implemention
template<typename RT, typename ...ArgT>
function<RT(ArgT...)> Y2(function<RT(function<RT(ArgT...)>, ArgT...)> f) {
    return [f](ArgT&& ...arg)->RT{ return f(Y2(f), forward<ArgT>(arg) ...); };
}

static function<int(int)> correctCase_Y2() {
    auto fib = Y2<int, int>([](function<int(int)> self, int x){
            if (x <= 1) return 1;
            else return self(x - 1) + self(x - 2);
            });
    printf("correctCase_Y2: %d\n", fib(7));
    return fib;
}
//////////////////////////////
// more implemention about Y-combinator, see Lisp/Y_Combinator...

//////////////////////////////
int main() {
    issueCase_fib();
    correctCase_objectBased();
    correctCase_Y();
    correctCase_Y2();
}
