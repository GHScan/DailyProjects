
#include "pch.h"
#include "base64.h"

Base64::Base64(const char *altChars) {
    mChar2Digit.resize(256, -1);

    for (int i = 0; i < 26; ++i) {
        char c = 'A' + i;
        mChar2Digit[c] = mDigit2Char.size();
        mDigit2Char.push_back(c);
    }
    for (int i = 0; i < 26; ++i) {
        char c = 'a' + i;
        mChar2Digit[c] = mDigit2Char.size();
        mDigit2Char.push_back(c);
    }
    for (int i = 0; i < 10; ++i) {
        char c = '0' + i;
        mChar2Digit[c] = mDigit2Char.size();
        mDigit2Char.push_back(c);
    }
    for (int i = 0; i < 2; ++i) {
        char c = altChars[i];
        assert(mChar2Digit[c] == -1);
        mChar2Digit[c] = mDigit2Char.size();
        mDigit2Char.push_back(c);
    }
}

int Base64::encode(void *_out, int _nout, const void *_str, int n) const {
    uint8_t *out = (uint8_t*)_out;
    const uint8_t *str = (const uint8_t*)_str;

    assert(str != nullptr && n > 0);
    int nout = (n + 2) / 3 * 4;
    if (out == nullptr) return nout;
    assert(nout == _nout);

    for (int i = 0; i < nout; i += 4) {
        out[0] = mDigit2Char[str[0] >> 2];
        out[1] = mDigit2Char[((str[0] & 0x3) << 4) | (str[1] >> 4)];
        out[2] = mDigit2Char[((str[1] & 0xf) << 2) | (str[2] >> 6)];
        out[3] = mDigit2Char[str[2] & 0x3f];
        out += 4;
        str += 3;
    }

    switch (n % 3) {
        case 1: out[-2] = '=';
        case 2: out[-1] = '=';
        case 0:
            break;
    }

    return nout;
}

string Base64::encode(const string &s) const {
    assert(!s.empty());

    string out(encode(nullptr, 0, s.c_str(), s.size()), 0);
    encode(&out[0], out.size(), s.c_str(), s.size());
    return out;
}

int Base64::decode(void *_out, int _nout, const void *_str, int n) const {
    uint8_t *out = (uint8_t*)_out;
    const uint8_t *str = (const uint8_t*)_str;

    assert(str != nullptr && n > 0 && n % 4 == 0);
    int nout = n / 4 * 3;
    if (str[n - 1] == '=') --nout;
    if (str[n - 2] == '=') --nout;
    if (out == nullptr) return nout;
    assert(nout == _nout);

    int digits[4];

    for (int i = 0; i + 3 <= nout; i += 3) {
        for (int j = 0; j < 4; ++j) {
            if ((digits[j] = mChar2Digit[str[j]]) == -1) return 0;
        }

        out[0] = (digits[0] << 2) | (digits[1] >> 4);
        out[1] = (digits[1] << 4) | (digits[2] >> 2);
        out[2] = (digits[2] << 6) | (digits[3]);
        str += 4;
        out += 3;
    }

    switch (nout % 3) {
        case 1:
            if ((digits[0] = mChar2Digit[str[0]]) == -1) return 0;
            if ((digits[1] = mChar2Digit[str[1]]) == -1) return 0;
            out[0] = (digits[0] << 2) | (digits[1] >> 4);
            break;
        case 2:
            if ((digits[0] = mChar2Digit[str[0]]) == -1) return 0;
            if ((digits[1] = mChar2Digit[str[1]]) == -1) return 0;
            if ((digits[2] = mChar2Digit[str[2]]) == -1) return 0;
            out[0] = (digits[0] << 2) | (digits[1] >> 4);
            out[1] = (digits[1] << 4) | (digits[2] >> 2);
            break;
        case 0:
            break;
    }

    return nout;
}

string Base64::decode(const string &s) const {
    assert(!s.empty() && s.size() % 4 == 0);

    string out(decode(nullptr, 0, s.c_str(), s.size()), 0);
    if (decode(&out[0], out.size(), s.c_str(), s.size())) return out;
    return "";
}
