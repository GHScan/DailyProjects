
#include "UnitTest.h"

static int _matchPattern(const char *pat, const char *str) {
    if (pat[0] == 0) return str[0] == 0;
    else {
        if (str[0] == 0) return 0;
        if (pat[0] == '*') {
            while (*str && !_matchPattern(pat + 1, str)) ++str;
            return *str != 0 || _matchPattern(pat + 1, str);
        } else {
            return pat[0] == str[0] && _matchPattern(pat + 1, str + 1);
        }
    }
}

int by_matchPattern(const char *pat, const char *str) {
    return _matchPattern(pat, str);
}
