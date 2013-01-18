// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cassert>

#include <windows.h>

const void* searchViaSunday(const void* _src, int srcLen, const void *_sub, int subLen)
{
    assert(_src != NULL && _sub != NULL);

    typedef unsigned char byte;
    const byte* src = (const byte*)_src;
    const byte* sub = (const byte*)_sub;
    const byte* srcEnd = src + srcLen - subLen;

    int jumps[256] = {0};
    for (int i = 0; i < _countof(jumps); ++i) jumps[i] = subLen + 1;
    for (int i = 0; i < subLen; ++i) jumps[sub[i]] = subLen - i;

    while (src <= srcEnd)
    {
        if (memcmp(src, sub, subLen) == 0) return src;
        int jump = jumps[src[subLen]];
        src += jump;
    }

    return NULL;
}

#include <ctime>

int main()
{
    srand(time(NULL));

    int c = 0, c2 = 0;
    {
        for (int z = 0; z < 1 << 4; ++z)
        {
            char buf[256] = "";
            char sub[48] = "";
            int n = rand() % 40 + 8;
            for (int i = 0; i < n; ++i) sub[i] = rand() % 4 + 'a';

            for (int i = 0; i < 1 << 15; ++i)
            {
                for (int i = 0; i < sizeof(buf) - 1; ++i) buf[i] = rand() % 4 + 'a';
                if (strstr(buf, sub) != NULL) ++c;
                if (searchViaSunday(buf, strlen(buf), sub, strlen(sub)) != NULL) ++c2;
                if (c != c2) { cout << "error" << endl; return 1; } 
            }
        }
    }
    
    cout << clock() << endl;
    cout << c << endl;
}