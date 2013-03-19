
#include "pch.h"

#include "SkipListArray.h"

int main()
{
    SkipListArray a(5);
    for (int i = 0; i < 10; ++i) a.insert(a.size(), i);
    
    int na[10];
    for (int i = 0; i < 10; ++i) na[i] = i;
    random_shuffle(na, na + 10);

    for (auto i : na) {
        printf("%d=%d,", i, a[i]);
    }
}
