
#include "pch.h"

#include <stdarg.h>
#include "Common.h"

string format(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    static char buf[256];
    vsprintf(buf, fmt, args);
    va_end(args);
    return buf;
}
