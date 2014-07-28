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

template<int n>
struct Identity {
    static const int value = n;
};

template<typename SourceT, int i>
struct Item;

template<typename SourceT, template<int> class PredT, int start>
struct FindBy {
    static const int value = If<PredT<Item<SourceT, start>::value>::value, Identity<start>, FindBy<SourceT, PredT, start + 1>>::Type::value;
};

template<typename SourceT, template<int> class PredT>
struct Filter { };
template<typename SourceT, template<int> class PredT, int i>
struct Item<Filter<SourceT, PredT>, i> {
    static const int _sourceIndex = FindBy<SourceT, PredT, Item<Filter<SourceT, PredT>, i - 1>::_sourceIndex + 1>::value;
    static const int value = Item<SourceT, _sourceIndex>::value;
};
template<typename SourceT, template<int> class PredT>
struct Item<Filter<SourceT, PredT>, 0> {
    static const int _sourceIndex = FindBy<SourceT, PredT, 0>::value;
    static const int value = Item<SourceT, _sourceIndex>::value;
};

template<typename SourceT, int begin, int count>
struct Foreach {
    template<typename FuncT>
    static void apply(FuncT f) {
        f(Item<SourceT, begin>::value);
        Foreach<SourceT, begin + 1, count - 1>::apply(f);
    }
};
template<typename SourceT, int begin>
struct Foreach<SourceT, begin, 0> {
    template<typename FuncT>
    static void apply(FuncT f) {
    }
};

template<int start>
struct Integers {};
template<int start, int i>
struct Item<Integers<start>, i> {
    static const int value = i + start;
};
//------------------------------
struct Primes {};
template<int i>
struct Item<Primes, i> {
    static const int value = Item<typename Item<Primes, i - 1>::RemainingSeq, 0>::value;

    template<int n>
    struct IsValid {
        static const bool value = n % Item<Primes, i>::value > 0;
    };
    typedef Filter<typename Item<Primes, i - 1>::RemainingSeq, IsValid > RemainingSeq;
};
template<>
struct Item<Primes, 0> {
    static const int value = 2;

    template<int n>
    struct IsValid {
        static const bool value = n % Item<Primes, 0>::value > 0;
    };
    typedef Filter<Integers<2>, IsValid > RemainingSeq;
};

int main() {
    Foreach<Primes, 0, 20>::apply([](int i){ cout << i << endl;});
}
