#include <iostream>
using namespace std;

struct True {
	static const int value = 1;
};
struct False {
	static const int value = 0;
};

template<bool, typename Conseq, typename Alt>
struct If {
	typedef Conseq Result;
};
template<typename Conseq, typename Alt>
struct If<false, Conseq, Alt> {
	typedef Alt Result;
};

struct Nil{};
template<typename Car, typename Cdr>
struct Cons {
	typedef Car Car;
	typedef Cdr Cdr;
};

template<typename List, typename Key>
struct Assq {
	typedef typename If<
		List::Car::Car::value == Key::value, 
		typename List::Car, 
		typename Assq<typename List::Cdr, Key>::Result>::Result Result;
};
template<typename Key>
struct Assq<Nil, Key> {
	typedef False Result;
};

template<typename List, typename Key> 
struct Lookup {
	typedef typename Assq<List, Key>::Result::Cdr Result;
};

template<typename Expr, typename Env>
struct Eval;

template<int _value>
struct Const {
	static const int value = _value;
};

template<int _value>
struct Var {
	static const int value = _value;
};

template<typename Formal, typename Body>
struct Lambda {
	typedef Formal Formal;
	typedef Body Body;
};

template<typename Func, typename Actual>
struct Application {
	typedef Func Func;
	typedef Actual Actual;
};

template<typename Lambda, typename Env>
struct Closure {
	template<typename Actual>
	struct Apply {
		typedef typename Eval<typename Lambda::Body, Cons<Cons<typename Lambda::Formal, Actual>, Env>>::Result Result;
	};
};

template<int value, typename Env>
struct Eval<Const<value>, Env> {
	typedef Const<value> Result;
};
template<int n, typename Env>
struct Eval<Var<n>, Env> {
	typedef typename Lookup<Env, Var<n>>::Result Result;
};
template<typename Formal, typename Body, typename Env>
struct Eval<Lambda<Formal, Body>, Env> {
	typedef Closure<Lambda<Formal, Body>, Env> Result;
};
template<typename Func, typename Actual, typename Env>
struct Eval<Application<Func, Actual>, Env> {
	typedef typename Eval<Func, Env>::Result  FuncResult;
	typedef typename Eval<Actual, Env>::Result ActualResult;
	typedef typename FuncResult::template Apply<ActualResult>::Result Result;
};

struct Add {
	template<typename T1>
	struct Apply {
		struct Result {
			template<typename T2>
			struct Apply {
				typedef Const<T1::value + T2::value> Result;
			};
		};
	};
};

int main() {
	typedef Var<0> x;
	typedef Var<1> y;
	typedef Var<2> z;
	typedef Var<3> add;
	typedef Cons<Cons<add, Add>, Nil> InitialEnv;

	cout 
		<< "((lambda (x) (+ 1 x)) 2)" 
		<< " ==> "
		<< Eval<
			Application<
				Lambda<x, 
					Application<Application<add, Const<1>>, x>>,
				Const<2>>,
			InitialEnv>::Result::value 
		<< endl;

	cout 
		<< "((lambda (x) (+ (+ 1 x) (+ 2 x))) 3)" 
		<< " ==> "
		<< Eval<
			Application<
				Lambda<x, 
					Application<
						Application<add, Application<Application<add, x>, Const<1>>>, 
						Application<Application<add, x>, Const<2>>>>,
				Const<3>>,
			InitialEnv>::Result::value 
		<< endl;
}
