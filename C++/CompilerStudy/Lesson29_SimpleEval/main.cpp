#include "pch.h" 

static bool parse_addExpr(const char *&str, double &r);
static bool parse_char(const char *&str, char c) {
    for (; isspace(str[0]); ) ++str;
    if (str[0] == c) return ++str, true;
    else return false;
}
static bool parse_number(const char *&str, double &r) {
    for (; isspace(str[0]); ) ++str;
    double sig = 1;
    for (; str[0] == '-'; ++str) sig = -sig;
    char *endstr;
    r = strtod(str, &endstr);
    if (endstr == NULL) return false;
    str = endstr;
    r *= sig;
    return true;
}
static bool parse_factor(const char *&str, double &r) {
    if (parse_number(str, r)) return true;
    return parse_char(str, '(') && parse_addExpr(str, r) && parse_char(str, ')');
}
static bool parse_mulExpr(const char *&str, double &r) {
    if (!parse_factor(str, r)) return false;
    double rv;
    for (;;) {
        if (parse_char(str, '*') && parse_factor(str, rv)) {
            r *= rv;
        } else if (parse_char(str, '/') && parse_factor(str, rv)) {
            r /= rv;
        } else return true;
    }
}
static bool parse_addExpr(const char *&str, double &r) {
    if (!parse_mulExpr(str, r)) return false;
    double rv;
    for (;;) {
        if (parse_char(str, '+') && parse_mulExpr(str, rv)) {
            r += rv;
        } else if (parse_char(str, '-') && parse_mulExpr(str, rv)) {
            r -= rv;
        } else return true;
    }
}
static bool eval(const char *str, double &r) {
    return parse_addExpr(str, r);
}

int main() {
    char buf[128];
    while (fgets(buf, sizeof buf, stdin)) {
        double v;
        if (eval(buf, v)) printf("%g\n", v);
        else puts("invalid expr");
    }
}
