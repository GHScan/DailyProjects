#include "pch.h"

#include <stdarg.h>

string format(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    static string s_buffer(256, 0);

    while (vsnprintf((char*)s_buffer.c_str(), s_buffer.size(), fmt, args) >= (int)s_buffer.size()) {
        s_buffer.resize(s_buffer.size() * 3 / 2);
    }
    s_buffer.resize(strlen(s_buffer.c_str()));

    va_end(args);
    return s_buffer;
}

string escapeString(const string &s, const char *specialChars) {
    string r;
    for (auto c : s) {
        switch (c) {
            case '\t':
                r.push_back('\\');
                r.push_back('t');
                break;
            case '\a':
                r.push_back('\\');
                r.push_back('a');
                break;
            case '\n':
                r.push_back('\\');
                r.push_back('n');
                break;
            case '\r':
                r.push_back('\\');
                r.push_back('r');
                break;
            case '\\':
                r.push_back('\\');
                r.push_back('\\');
                break;
            default:
                if (strchr(specialChars, c)) {
                    r.push_back('\\');
                }
                r.push_back(c);
                break;
        }
    }
    return r;
}

string unescapeString(const string &s) {
    string r;
    for (int i = 0; i < (int)s.size(); ++i) {
        if (s[i] == '\\') {
            switch (s[++i]) {
                case 'a':
                    r.push_back('\a');
                    break;
                case 't':
                    r.push_back('\t');
                    break;
                case 'n':
                    r.push_back('\n');
                    break;
                case 'r':
                    r.push_back('\r');
                    break;
                default:
                    r.push_back(s[i]);
                    break;
            }
        } else {
            r.push_back(s[i]);
        }
    }
    return r;
}
