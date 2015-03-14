#include "stdafx.h"
//------------------------------
#include <ctype.h>

class Solution {
public:
    bool isNumber(string const &s) {
        char occurs[256] = {0};
        for (auto c : s) occurs[c] = 1;
        if (occurs['e'] || occurs['E']) return isScientificFloat(s);
        else if (occurs['.']) return isFloat(s);
        else return isInteger(s);
    }
private:
    bool parseWhitespace(const char *&p) {
        if (!isspace(*p)) return false;
        for (; isspace(*p); ++p);
        return true;
    }
    bool parseSign(const char *&p) {
        return *p == '+' || *p == '-' ? (++p, true) : false;
    }
    bool parseDigits(const char *&p) {
        if (!isdigit(*p)) return false;
        for (; isdigit(*p); ++p);
        return true;
    }
    bool parseHexs(const char *&p) {
        if (*p != '0' || (p[1] != 'x' && p[1] != 'X') ||
            !(isdigit(p[2]) || (p[2] >= 'a' && p[2] <= 'f') || (p[2] >= 'A' && p[2] <= 'F'))) return false;
        p += 2;
        for (; isdigit(*p) || (*p >= 'a' && *p <= 'f') || (*p >= 'A' && *p <= 'F'); ++p);
        return true;
    }
    bool parseFloat(const char *&p) {
        const char *temp = p;
        bool foundDigits = parseDigits(temp);
        if (*temp == '.') ++temp;
        foundDigits = parseDigits(temp) || foundDigits;
        if (foundDigits) p = temp;
        return foundDigits;
    }

    bool isInteger(string const &s) {
        const char *p = s.c_str();
        parseWhitespace(p);
        parseSign(p);
        if (!parseHexs(p) && !parseDigits(p)) return false;
        parseWhitespace(p);
        return *p == 0;
    }
    bool isFloat(string const &s) {
        const char *p = s.c_str();
        parseWhitespace(p);
        parseSign(p);
        if (!parseFloat(p)) return false;
        parseWhitespace(p);
        return *p == 0;
    }
    bool isScientificFloat(string const &s) {
        const char *p = s.c_str();
        parseWhitespace(p);

        parseSign(p);
        if (!parseFloat(p)) return false;
        if (*p != 'e' && *p != 'E') return false;
        ++p;
        parseSign(p);
        if (!parseDigits(p)) return false;

        parseWhitespace(p);
        return *p == 0;
    }
};
//------------------------------

int main() {
    Solution s;
    string strs[] = {
          " 0 ", " 324 ",  " -234 ", " 0234", " 0x2abf4 ",
          " 0a ", "e 324 ", " -234 324 ", " 0294", " 0x2abfh ",
          " +234.234 ", " -.23423 ", " 234. ", 
          " 0234.234 ", " 0x234.234 ", " --.23423 ", " . ",
          " 3.14e234", " -3.14E+234", " -3.14E+234.234", " -3.14E+.234234",
          " 3.14e 234", " -03.14E.234", " -3.14E",

          "0", " 0.1 ", "abc", "1 a", "2e10", " 3. ", " .00 ", " 008 ", " 6e6.5 ", 
    };
    for (auto &str : strs) cout << str << "=>" << s.isNumber(str) << endl;
}
