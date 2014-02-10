#include "pch.h"

#include <functional>
#include <memory>
#include <type_traits>

template<typename T>
class LazyValue {
private:
    struct LazyValueImpl {
        mutable function<T()> m_f;
        mutable bool m_hasEval;
        mutable T m_value;
        LazyValueImpl(function<T()> f): m_f(f), m_hasEval(false){}
        T& value() const {
            if (!m_hasEval) {
                m_value = m_f();
                m_f = 0;
                m_hasEval = true;
            }
            return m_value;
        }
    };
public:
    LazyValue(){}
    LazyValue(function<T()> f): m_impl(make_shared<LazyValueImpl>(f)){ }
    T& value() const {
        return m_impl->value();
    }
private:
    shared_ptr<LazyValueImpl> m_impl;
};
template<typename FuncT>
LazyValue<typename result_of<FuncT&()>::type> lazy(const FuncT& f) {
    return LazyValue<typename result_of<FuncT&()>::type>(f);
}

//////////////////////////////
int func() {
     printf("call %s:%d\n", __func__, __LINE__); 
     return 3;
}
class HeavyObj {
public:
    HeavyObj(){ puts("construct heavy");}
    ~HeavyObj(){ puts("destruct heavy"); }
    int value() { return 10; }
private:
};

int main() {
    auto v = lazy(func);
    LazyValue<int> v2([v](){ 
            printf("call %s:%d\n", __func__, __LINE__);
            return 5 * v.value(); 
            });
    LazyValue<int> v3;
    {
        auto obj = make_shared<HeavyObj>();
        v3 = lazy([v, v2, obj](){ 
                printf("call %s:%d\n", __func__, __LINE__);
                return v.value() * v2.value() * obj->value(); 
                });
    }


    puts("--- before");
    cout << v3.value() << endl;
    puts("--- after");
    cout << v3.value() << endl;
    cout << v3.value() << endl;
}
