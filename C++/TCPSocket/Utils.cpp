#include "pch.h"

#include <stdarg.h>

#include "Utils.h"

const char *_ENSURE_CTX_A = "";
const char *_ENSURE_CTX_B = "";

Exception::Exception(const char *describ, const char *file, int line) {
    mWhat = format("%s(%d): %s\n", file, line, describ);
}
Exception::Exception() {
}

PosixException::PosixException(int err, const char *describ, const char *file, int line) {
    mWhat = format("%s(%d): %s\n\t%s\n", file, line, describ, strerror(err));
}

string format(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    string v(256, 0);
    while (vsnprintf((char*)v.c_str(), v.size(), fmt, args) == (int)v.size()) {
        v.resize(v.size() * 3 / 2);
    }
    v.resize(strlen(v.c_str()));
    va_end(args);
    return v;
}
