 
#include "pch.h"

#include <stdint.h>

#include <sstream>

#include "Utils.h"

class BigInteger {
private:
    typedef uint8_t     T;
    typedef uint32_t    DoubleT;
    static const DoubleT BASE = DoubleT(1) << (sizeof(T) * 8);
    typedef vector<T> Buffer;
public:
    BigInteger(): mBuf(1, 0){}
    BigInteger(uint32_t i) {
        fromUint(mBuf, i);
    }
    BigInteger operator + (const BigInteger &o) {
        BigInteger r;
        add(r.mBuf, mBuf, o.mBuf);
        return move(r);
    }
    BigInteger operator * (const BigInteger &o) {
        BigInteger r;
        mul(r.mBuf, mBuf, o.mBuf);
        return move(r);
    }
    bool operator == (const BigInteger &o) const { return compare(mBuf, o.mBuf) == 0; }
    bool operator != (const BigInteger &o) const { return !(*this == o); }
    bool operator < (const BigInteger &o) const { return compare(mBuf, o.mBuf) < 0; }
    bool operator <= (const BigInteger &o) const { return !(o < *this); }
    bool operator > (const BigInteger &o) const { return o < *this; }
    bool operator >= (const BigInteger &o) const { return !(*this < o); }
    friend ostream& operator << (ostream &so, const BigInteger& i) {
        so << toString(i.mBuf);
        return so;
    }
private:
    static void fromUint(Buffer& buf, uint32_t i) {
        for (; i >= BASE; i /= BASE) buf.push_back(i % BASE);
        buf.push_back(i);
    }
    static string toString(const Buffer &buf) {
        uint32_t n = 1, divisor = 10;
        for (; divisor * 10 < BASE; divisor *= 10, ++n);

        ostringstream so;

        Buffer tmpBuf(buf);
        for (; tmpBuf.size() > 1 || tmpBuf[0] >= divisor; ) {
            T remain = div(tmpBuf, divisor);
            for (auto i = 0u; i < n; ++i, remain /= 10) so << char('0' + remain % 10);
        }
        for (; tmpBuf.front() >= 10; tmpBuf.front() /= 10) so << char('0' + tmpBuf.front() % 10);
        so << char('0' + tmpBuf.front());

        string s = so.str();
        reverse(s.begin(), s.end());
        return s;
    }
    static void add(Buffer& result, const Buffer &x, const Buffer &y) {
        assert(isNormalized(x) && isNormalized(y));
        if (x.size() < y.size()) {
            add(result, y, x);
            return;
        }

        result.assign(x.size() + 1, T());

        DoubleT carry = 0;
        auto i = 0u;
        for (; i < y.size(); ++i) {
            carry = carry + x[i] + y[i];
            result[i] = carry % BASE;
            carry /= BASE;
        }
        for ( ;i < x.size(); ++i) {
            carry = carry + x[i];
            result[i] = carry % BASE;
            carry /= BASE;
        }
        result[i] = carry;

        normlize(result);
    }
    static void mulTo(Buffer &result, int roff, const Buffer &x, T y) {
        assert(isNormalized(x));
        assert(result.size() - roff > x.size());

        DoubleT carry = 0;
        auto i = 0u;
        for (; i < x.size(); ++i) {
            carry = carry + DoubleT(x[i]) * y + result[roff + i];
            result[roff + i] = carry % BASE;
            carry /= BASE;
        }
        for (i += roff; i < result.size() && carry > 0; ++i) {
            carry += result[i];
            result[i] = carry % BASE;
            carry /= BASE;
        }
        assert(carry == 0);
    }
    static void mul(Buffer &result, const Buffer &x, const Buffer &y) {
        assert(isNormalized(x) && isNormalized(y));
        if (x.size() < y.size()) {
            add(result, y, x);
            return;
        }

        result.assign(x.size() + y.size(), T());

        for (auto i = 0u; i < y.size(); ++i) {
            mulTo(result, i, x, y[i]);
        }

        normlize(result);
    }
    static T div(Buffer &result, T v) {
        assert(isNormalized(result));

        DoubleT remain = 0;
        for (int i = result.size() - 1; i >= 0; --i) {
            remain = remain * BASE + result[i];
            result[i] = remain / v;
            remain %= v;
        }

        normlize(result);
        return remain;
    }
    static int compare(const Buffer &x, const Buffer &y) {
        assert(isNormalized(x) && isNormalized(y));
        if (x.size() != y.size()) {
            return x.size() < y.size() ? -1 : 1;
        } else {
            auto i = x.size() - 1;
            for (; i > 0 && x[i] == y[i]; --i);
            if (x[i] != y[i]) {
                return x[i] < y[i] ? -1 : 1;
            } else return 0;
        }
    }
    static bool isNormalized(const Buffer &buf) {
        return buf.back() != 0 || buf.size() == 1;
    }
    static void normlize(Buffer &buf) {
        while (buf.size() > 1 && buf.back() == 0) buf.pop_back();
    }

private:
    Buffer mBuf;
};

int main() {
    BigInteger big(1);
    for (BigInteger i = 1; i < 100; i = i + 1) {
        big = big * i;
        cout << big << endl;
    }
}
