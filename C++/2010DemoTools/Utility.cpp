#include "StdAfx.h"

#include <stdarg.h>
#include <limits>

#include "Utility.h"

namespace Scan
{   
    OStringStream& OutStream::getStream()
    {
        return m_os;
    }

    OutStream& OutStream::format(const char *fmt, ...)
    {
        va_list arg;
        va_start(arg, fmt);

        int len = _vscprintf(fmt, arg) + 1;
        char *buf = new char[len];
        vsprintf_s(buf, len, fmt, arg);
        m_os << buf;
        delete[] buf;

        va_end(arg);

        return *this;
    }

    NativeLocale::NativeLocale()
    {
        m_lastLocale = setlocale(LC_ALL, "");
    }

    NativeLocale::~NativeLocale()
    {
        setlocale(LC_ALL, m_lastLocale);
    }

    const float FLOAT_EPSILON = std::numeric_limits<float>::epsilon();
    const float FLOAT_INFINITY = std::numeric_limits<float>::infinity();
}   