#include "pch.h" 

#include "sort.h"
#include "map.h"

int main() {
    typedef iList<3,1,5,4,2,8,4,3,1,2>::type List;

    puts("sort with qsort:");
    iForeach<Qsort<List>::type>::apply([](int i){ cout << i << ','; });
    puts("");

    puts("sort with map:");
    iForeach<iMap2iList<iList2iMap<List>::type>::type>::apply([](int i){ cout << i << ','; });
    puts("");
}
