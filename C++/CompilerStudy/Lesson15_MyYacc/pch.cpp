#include "pch.h"

#include <stdarg.h>

string format(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    static char buf[512] = "";
    vsprintf(buf, fmt, args);
    va_end(args);
    return buf;
}

string readFile(const string& fname)
{
    ifstream fi(fname.c_str());
    string r;
    for (string line; getline(fi, line); r += line + '\n');
    return r;
}
