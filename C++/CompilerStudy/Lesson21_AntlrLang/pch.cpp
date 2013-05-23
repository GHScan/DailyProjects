#include "pch.h"
#include <stdarg.h>

string format(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    static vector<char> buf(256);
    while ((int)buf.size() == vsnprintf(&buf[0], buf.size(), fmt, args)) {
        buf.resize(buf.size() * 3 / 2);
    }
    va_end(args);
    return &buf[0];
}

string tabString(int n) {
    string r;
    for (int i = 0; i < n * 4; ++i) r.push_back(' ');
    return r;
}

#include "pch.h"
