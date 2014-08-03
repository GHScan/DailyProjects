#ifndef MAP_H
#define MAP_H

#include "list.h"

template<int _value, typename _left, typename _right>
struct iMapNode {
    static const int value = _value;
    typedef _left left;
    typedef _right right;
};

template<typename MapNodeT, int v>
struct iInsertMapNode {
    typedef typename 
        If<(v <= MapNodeT::value), 
        iMapNode<MapNodeT::value, typename iInsertMapNode<typename MapNodeT::left, v>::type, typename MapNodeT::right>,
        iMapNode<MapNodeT::value, typename MapNodeT::left, typename iInsertMapNode<typename MapNodeT::right, v>::type>
            >::type type;
};
template<int v>
struct iInsertMapNode<Nil, v> {
    typedef iMapNode<v, Nil, Nil> type;
};

template<typename MapNodeT>
struct iMap2iList {
    typedef typename iMap2iList<typename MapNodeT::left>::type _left;
    typedef typename iMap2iList<typename MapNodeT::right>::type _right;
    typedef typename iAppend<_left, iCons<MapNodeT::value, _right>>::type type;
};
template<>
struct iMap2iList<Nil> {
    typedef Nil type;
};

template<typename ConsT>
using iList2iMap = iFoldr<ConsT, Nil, iInsertMapNode>;

#endif
