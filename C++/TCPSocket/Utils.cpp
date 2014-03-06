#include "pch.h"

#include <stdarg.h>

#include <execinfo.h>

#include "Utils.h"

const char *_ASSERT_VAR_A = "";
const char *_ASSERT_VAR_B = "";

LogicError::LogicError(const char *describ, const char *file, int line)
    : Throwable(constructErrorMsg(describ, file, line).c_str()){
}
string LogicError::constructErrorMsg(const char *describ, const char *file, int line) {
    return format("%s(%d): %s\n", file, line, describ) + stackTrace();
}
string LogicError::stackTrace() {
    string ret;

    void *frames[32];
    int n = ::backtrace(frames, ARRAY_SIZE(frames));
    char **strs = ::backtrace_symbols(frames, n);
    for (int i = 0; i < n; ++i) {
        ret += strs[i]; 
        ret += '\n';
    }
    free(strs);

    return ret;
}

PosixException::PosixException(int err, const char *describ, const char *file, int line)
    : RuntimeException(format("%s(%d): %s\n\t%s\n", file, line, describ, strerror(err)).c_str()) {
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
