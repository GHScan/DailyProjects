
#include "pch.h"
#include <stdarg.h>

string format(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    static char buf[256];
    vsprintf(buf, fmt, args);
    va_end(args);
    return buf;
}

string tabString(int n)
{
    string r;
    for (int i = 0; i < n * 4; ++i) r.push_back(' ');
    return r;
}
