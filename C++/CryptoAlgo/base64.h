#ifndef BASE64_H
#define BASE64_U

#include <string>

class Base64 {
public:
    explicit Base64(const char *altChars = "+/");

    int encode(void *out, int nout, const void *str, int n) const;
    string encode(const string &s) const;

    int decode(void *out, int nout, const void *str, int n) const;
    string decode(const string &s) const;
private:
    string mDigit2Char;
    string mChar2Digit;
};
 
#endif
