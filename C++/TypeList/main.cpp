#include "pch.h"

#include <functional>

struct YesType {
    char c[2];
};
struct NoType {
    char c[1];
};

template<typename T1, typename T2>
struct IsBaseOf {
    static YesType _test(T1*);
    static NoType _test(...);
    enum {
        Value = sizeof(_test((T2*)0)) == sizeof(YesType),
    };
};

template<typename T1, typename T2>
struct IsSame {
    enum {
        Value = IsBaseOf<T1, T2>::Value && IsBaseOf<T2, T1>::Value,
    };
};

//------------------------------
struct TypeNodeNil {};

template<typename _ValueT, typename _NextT>
struct TypeNode {
    typedef _ValueT ValueT;
    typedef _NextT NextT;
};

template<typename T, typename ...Tn>
struct Create {
    typedef TypeNode<T, typename Create<Tn...>::Type> Type;
};
template<typename T>
struct Create<T> {
    typedef TypeNode<T, TypeNodeNil> Type;
};

template<typename TypeNode>
struct Length {
    enum {
        Value = 1 + Length<typename TypeNode::NextT>::Value,
    };
};
template<>
struct Length<TypeNodeNil> {
    enum {
        Value = 0,
    };
};

template<typename TypeNode, int i>
struct Get {
    typedef typename Get<typename TypeNode::NextT, i - 1>::Type Type;
};
template<typename TypeNode>
struct Get<TypeNode, 0> {
    typedef typename TypeNode::ValueT Type;
};

template<typename NodeType, int begin, int end>
struct Sub {
    typedef typename Sub<typename NodeType::NextT, begin - 1, end - 1>::Type Type;
};
template<typename NodeType, int end>
struct Sub<NodeType, 0, end> {
    typedef TypeNode<typename NodeType::ValueT, typename Sub<typename NodeType::NextT, 0, end - 1>::Type> Type;
};
template<typename NodeType>
struct Sub<NodeType, 0, 1> {
    typedef TypeNode<typename NodeType::ValueT, TypeNodeNil> Type;
};

template<typename NodeType, typename Type>
struct IndexOf {
    enum {
        _Value = IsSame<typename NodeType::ValueT, Type>::Value ? 0 : 1 + IndexOf<typename NodeType::NextT, Type>::_Value,
        Value = _Value < 0 ? -1 : _Value,
    };
};
template<typename Type>
struct IndexOf<TypeNodeNil, Type> {
    enum {
        _Value = -1024,
    };
};
//------------------------------

template<typename T>
struct TypeName {
    static const char* name() {
        return typeid(T).name();
    }
};

int main() {
    typedef Create<char, int, long, float, double, void*>::Type ListT;
    cout << Length<ListT>::Value << endl;
    cout << TypeName<Get<ListT, 0>::Type>::name() << endl;
    cout << TypeName<Get<ListT, 1>::Type>::name() << endl;
    cout << TypeName<Get<ListT, 2>::Type>::name() << endl;
    cout << TypeName<Get<ListT, 3>::Type>::name() << endl;
    cout << TypeName<Get<ListT, 4>::Type>::name() << endl;
    cout << TypeName<Get<ListT, 5>::Type>::name() << endl;
    cout << IndexOf<ListT, int>::Value << endl;
    cout << IndexOf<ListT, double>::Value << endl;
    cout << IndexOf<ListT, void*>::Value << endl;
    cout << IndexOf<ListT, int*>::Value << endl;

    typedef Sub<ListT, 3, Length<ListT>::Value>::Type List2;
    cout << Length<List2>::Value << endl;
    cout << TypeName<Get<List2, 0>::Type>::name() << endl;
    cout << TypeName<Get<List2, 1>::Type>::name() << endl;
    cout << TypeName<Get<List2, 2>::Type>::name() << endl;
    cout << IndexOf<List2, double>::Value << endl;
    cout << IndexOf<List2, void*>::Value << endl;
    cout << IndexOf<List2, int*>::Value << endl;
}
