#ifndef SORT_H
#define SORT_H

#include "list.h"

template<typename ConsT>
struct Qsort {
    template<int v> struct _iLess { static const bool value = v < ConsT::car;};
    template<int v> struct _iGreaterEq { static const bool value = v >= ConsT::car;};
    typedef typename Qsort<typename iFilter<typename ConsT::cdr, _iLess>::type>::type _smaller;
    typedef typename Qsort<typename iFilter<typename ConsT::cdr, _iGreaterEq>::type>::type _greater;
    typedef typename iAppend<_smaller, iCons<ConsT::car, _greater>>::type type;
};
template<int car>
struct Qsort<iCons<car, Nil>> {
    typedef iCons<car, Nil> type;
};
template<>
struct Qsort<Nil> {
    typedef Nil type;
};

#endif
