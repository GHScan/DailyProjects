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

string unEscape(const string& s) {
    string r;
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[i + 1]) {
            case 'a': r.push_back('\a'); break;
            case 'n': r.push_back('\n'); break;
            case 'r': r.push_back('\r'); break;
            case 't': r.push_back('\t'); break;
            default: r.push_back(s[i + 1]); break;
            }
            ++i;
        } else {
            r.push_back(s[i]);
        }
    }
    return r;
}
string escape(const string &s) {
    string r;
    for (int i = 0; i < (int)s.size(); ++i) {
        if (isgraph(s[i]) && !isspace(s[i])) r += s[i];
        else r += format("\\x%02x", s[i]);
    }
    return r;
}
