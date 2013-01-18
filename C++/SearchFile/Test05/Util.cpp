#include "stdafx.h"

#include "stdarg.h"

#include "Util.h"

std::string format(const char *fmt, ...)
{
    std::string r;

    va_list args;
    va_start(args, fmt);

    r.resize(_vscprintf(fmt, args) + 1);
    vsprintf_s((char*)r.c_str(), r.size(), fmt, args);

    va_end(args);

    return r;
}