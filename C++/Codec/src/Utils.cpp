#include <cstdio>

#include "Utils.h"

void DumpNumbers(float const *src, int n, int lineW)
{
    for (int i = 0; i < n; ++i)
    {
        printf("%10.4f,", src[i]);

        if ((i + 1) % lineW == 0)
        {
            puts("");
        }
    }
}

void DumpNumbers(uint8_t const *src, int n, int lineW)
{
    for (int i = 0; i < n; ++i)
    {
        printf("%02d,", src[i]);

        if ((i + 1) % lineW == 0)
        {
            puts("");
        }
    }
}